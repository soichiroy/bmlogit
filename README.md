
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

``` r
## load data
data(cces_nc)
data(acs_nc)

## population data
# target_Y <- acs_nc %>% count(educ, wt = N) %>% mutate(prop = n / sum(n)) %>% pull(prop)
target_Y <- c(0.462, 0.258, 0.184, 0.096)

pop_X <- model.matrix(~age + gender, data = acs_nc %>% count(age, gender, wt = N))[,-1]
count_N <- acs_nc %>% count(age, gender, wt = N) %>% pull(n)

## survey data
Y <- model.matrix(~educ-1, data = cces_nc)
X <- model.matrix(~age + gender, data = cces_nc)[,-1]

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
those levels: 5 levels of age and 1 for gender.

<img src="man/figures/README-coef-bmlogit-emlogit-1.png" width="100%" />

We can compare the resulting predicted values. Each prediction from the
regression is made for each X covariate, and then summed with weights
according to their known counts.

|              | Target | bmlogit | emlogit |   Raw |
|:-------------|-------:|--------:|--------:|------:|
| HS or Less   |  0.462 |   0.460 |   0.295 | 0.294 |
| Some College |  0.258 |   0.258 |   0.354 | 0.355 |
| 4-Year       |  0.184 |   0.186 |   0.231 | 0.231 |
| Post-Grad    |  0.096 |   0.096 |   0.120 | 0.120 |

## Application to Post-Stratification

We now try to estimate turnout poststratifying on a synthetic
distribution that was created by either emlogit, bmlogit, or simple
weights.

The prediction for a third variable, `Y` (turnout in this case) should
go like this:

``` r
#' @param pop Population targets, with variables called `weight`
pred_turnout <- function(pop, microdata = cces_nc, XZ = c("educ", "age", "gender")) {
  
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
  count(educ, age, gender, wt = N) %>%
  mutate(weight = n / sum(n))

turnout_nonpar <- pred_turnout(pop_tab)
```

For bmlogit:

``` r
popX_df <- count(acs_nc, age, gender, wt = N) %>% 
  transmute(age, gender, prop_X = n / sum(n))

# compute the joint table
pr_joint <- predict(fit, newdata = pop_X)
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")

pop_bmlogit <- bind_cols(popX_df, as_tibble(pr_joint)) %>%
  pivot_longer(cols = -c(age, gender, prop_X),
               names_to = "educ", values_to = "pr") %>% 
  mutate(weight = prop_X * pr) # Pr(X) * Pr(Z | X)

turnout_bmlogit <- pred_turnout(pop_bmlogit)
```

For emlogit

``` r
pr_joint <- predict(fit_em, newdata = pop_X)
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")

pop_emlogit <- bind_cols(popX_df, as_tibble(pr_joint)) %>%
  pivot_longer(cols = -c(age, gender, prop_X),
               names_to = "educ", values_to = "pr") %>% 
  mutate(weight = prop_X * pr) # Pr(X) * Pr(Z | X)

turnout_emlogit <- pred_turnout(pop_emlogit)
```

And we finally compare all four. The true statewide turnout is 0.596.

| Estimator                  | Estimate |  Error |
|:---------------------------|---------:|-------:|
| Sample Mean                |    0.575 | -0.022 |
| Post-str. w/ True Joint    |    0.560 | -0.037 |
| Post-str. w/ bmlogit Joint |    0.553 | -0.043 |
| Post-str. w/ emlogit Joint |    0.569 | -0.027 |
