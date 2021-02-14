
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bmlogit

<!-- badges: start -->
<!-- badges: end -->

The goal of `bmlogit` is to estimate the multinominal logistic
regression with prediction constraints.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soichiroy/bmlogit")
```

## Implementation

``` r
library(bmlogit)
library(emlogit)
library(synthArea)
library(tidyr)
library(stringr)
```

Let’s use the synthArea but add another Census covariate – urban/rural.
We use the
[citylab](https://github.com/theatlantic/citylab-data/blob/master/citylab-congress/citylab_cdi.csv)
index to classify NC’s 13 CDs into three categories

``` r
## load data
data(cces_nc)
data(acs_nc)

urban_recode <- function(cd) {
  case_when(
    cd %in% c("NC-03", "NC-07", "NC-08", "NC-11") ~ "Rural",
    cd %in% c("NC-01", "NC-02", "NC-05", "NC-06", "NC-10", "NC-13") ~ "Rural-suburban", 
    cd %in% c("NC-04", "NC-09", "NC-12") ~ "Sparse-suburban"
  )
}
cces_nc$rural <- urban_recode(cces_nc$cd)
acs_nc$rural <- urban_recode(acs_nc$cd)
```

``` r
## population data
# target_Y <- acs_nc %>% count(educ, wt = N) %>% mutate(prop = n / sum(n)) %>% pull(prop)
target_Y <- c(0.462, 0.258, 0.184, 0.096)

pop_X <- model.matrix(~age + gender + rural, data = acs_nc %>% count(age, gender, rural, wt = N))[,-1]
count_N <- acs_nc %>% count(age, gender, rural, wt = N) %>% pull(n)

## survey data
Y <- model.matrix(~educ-1, data = cces_nc)
X <- model.matrix(~age + gender + rural, data = cces_nc)[,-1]

## fit
fit <- bmlogit(Y = Y, X = X, 
               target_Y = target_Y, 
               pop_X = pop_X, 
               count_X = count_N,
               control = list(tol_pred = 0.005))


## multinominal logit without constraints
fit_em <- emlogit(Y = Y, X = X)
```

There is a set multinomial coefficients for each level of the outcome
(relative to baseline). Here there are six coefficients for each of
those levels: 5 levels of age, 1 for gender, and 3 for rurality.

<img src="man/figures/README-coef-bmlogit-emlogit-1.png" width="100%" />

We can compare the resulting predicted values. Each prediction from the
regression is made for each X covariate, and then summed with weights
according to their known counts.

|              | Target | bmlogit | emlogit |   Raw |
|:-------------|-------:|--------:|--------:|------:|
| HS or Less   |  0.462 |   0.460 |   0.297 | 0.294 |
| Some College |  0.258 |   0.259 |   0.354 | 0.355 |
| 4-Year       |  0.184 |   0.186 |   0.230 | 0.231 |
| Post-Grad    |  0.096 |   0.096 |   0.119 | 0.120 |

We clearly see that bmlogit is closer to the target population, and
emlogit defaults to the raw data.

## Application to Post-Stratification and Estimatig Turnout

We now try to estimate turnout poststratifying on a synthetic
distribution that was created by either emlogit, bmlogit, or simple
weights.

The prediction for a third variable, `Y` (turnout in this case) should
go like this:

``` r
#' @param pop Population targets, with variables called `weight`
pred_turnout <- function(pop, microdata = cces_nc, XZ = c("educ", "age", "gender", "rural")) {
  
  XZw <- microdata %>% 
    group_by(!!!syms(XZ)) %>%
    summarise(turnout = sum(vv_turnout) / n(), .groups = "drop") %>%
    # join
    left_join(pop, by = XZ) %>%
    # rescale
    mutate(weight = weight / sum(weight))
  
  # inner product
  XZw$weight %*% XZw$turnout
}
```

We first do a simple weighted mean for the full joint (which we know in
this case).

``` r
# estimate state-wide quantity
data(districts_nc)
data(estimands_nc)

## turnout in NC
turnout <- sum(estimands_nc$totalvotes) / sum(estimands_nc$vap)

## raw estimate without adjustment
turnout_naive <- mean(cces_nc$vv_turnout)

## estimates stratified on educ, age, and gender
pop_tab <- acs_nc %>% 
  count(educ, age, gender, rural, wt = N) %>%
  mutate(weight = n / sum(n))

turnout_nonpar <- pred_turnout(pop_tab)
```

For bmlogit:

``` r
popX_df <- acs_nc %>% 
  count(age, gender, rural, wt = N) %>% 
  transmute(age, gender, rural, prop_X = n / sum(n))

# compute the joint table
pr_joint <- predict(fit, newdata = pop_X)
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")

pop_bmlogit <- bind_cols(popX_df, as_tibble(pr_joint)) %>%
  pivot_longer(cols = -c(age, gender, rural, prop_X),
               names_to = "educ", values_to = "pr") %>% 
  mutate(weight = prop_X * pr) # Pr(X) * Pr(Z | X)

turnout_bmlogit <- pred_turnout(pop_bmlogit)
```

For emlogit

``` r
pr_joint <- predict(fit_em, newdata = pop_X)
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")

pop_emlogit <- bind_cols(popX_df, as_tibble(pr_joint)) %>%
  pivot_longer(cols = -c(age, gender, rural, prop_X),
               names_to = "educ", values_to = "pr") %>% 
  mutate(weight = prop_X * pr) # Pr(X) * Pr(Z | X)

turnout_emlogit <- pred_turnout(pop_emlogit)
```

And we finally compare all four. The true statewide turnout is 0.596.

| Estimator                  | Estimate |  Error |
|:---------------------------|---------:|-------:|
| Sample Mean                |    0.575 | -0.022 |
| Post-str. w/ True Joint    |    0.562 | -0.034 |
| Post-str. w/ bmlogit Joint |    0.555 | -0.041 |
| Post-str. w/ emlogit Joint |    0.574 | -0.023 |
