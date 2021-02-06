
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cmlogit

<!-- badges: start -->

<!-- badges: end -->

The goal of `cmlogit` is to estimate the multinominal logistic
regression with prediction constraints.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soichiroy/cmlogit")
```

## Example

``` r
library(cmlogit)
library(synthArea)
```

``` r
## load data
data(cces_nc)
data(acs_nc)

## population data
popY <- acs_nc %>% count(educ, wt = N) %>% mutate(prop = n / sum(n)) %>% pull(prop)
popX <- model.matrix(~age + gender, data = acs_nc %>% count(age, gender, wt = N))
popN <- acs_nc %>% count(age, gender, wt = N) %>% pull(n)

## survey data
Y <- model.matrix(~educ-1, data = cces_nc)
X <- model.matrix(~age + gender, data = cces_nc)

## fit
fit <- cmlogit(Y = Y, X = X, popY = popY, popX = popX, popN = popN,
               control = list(intercept = FALSE, tol_pred = 0.01))


## result
bb0 <- cbind(0, matrix(fit$fit$x0, nrow = ncol(X)))
bbs <- cbind(0, matrix(fit$fit$solution, nrow = ncol(X)))

res <- cbind(popY,
  apply(popX %*% bbs, 1, function(x) exp(x) / sum(exp(x))) %*% (popN / sum(popN)),
  apply(popX %*% bb0, 1, function(x) exp(x) / sum(exp(x))) %*% (popN / sum(popN)),
  colMeans(Y)
)

knitr::kable(res, col.names = c("population", "cmlogit", "emlogit", "raw prop."),
             digits = 4)
```

|                  | population | cmlogit | emlogit | raw prop. |
| :--------------- | ---------: | ------: | ------: | --------: |
| educHS or Less   |     0.3943 |  0.3893 |  0.2953 |    0.2941 |
| educSome College |     0.3266 |  0.3266 |  0.3536 |    0.3552 |
| educ4-Year       |     0.1835 |  0.1864 |  0.2310 |    0.2311 |
| educPost-Grad    |     0.0956 |  0.0977 |  0.1201 |    0.1196 |

``` r
## coefficients
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), pty = 's')
for (i in 1:3) {
  plot(bb0[,i+1], bbs[,i+1], pch = 16,
       xlab = "emlogit", ylab = "cmlogit",
       main = rownames(res)[i+1])
  abline(0, 1, col = 'gray', lty = 2)  
}
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Application to Post-Stratification

``` r
# estimate state-wide quantity 
data("districts_nc")
data(estimands_nc)

## turnout in NC 
turnout <- sum(estimands_nc$totalvotes) / sum(estimands_nc$vap)

## raw estimate without adjustment 
turnout_naive <- mean(cces_nc$vv_turnout)

## estimates stratified on educ, age, and gender 
pop_tab <- acs_nc %>% count(educ, age, gender, wt = N) %>% 
    mutate(weight = n / sum(n))
nonpar <- cces_nc %>% group_by(educ, age, gender) %>% 
  summarise(turnout = sum(vv_turnout) / n(), .groups = "drop") %>%
  left_join(pop_tab, by = c("educ", "age", "gender")) %>% 
  mutate(scale_weight = weight / sum(weight)) 
turnout_nonpar <- nonpar$scale_weight %*% nonpar$turnout


## cmlogit 
pop_tab2 <- acs_nc %>% count(age, gender, wt = N) %>% mutate(prop = n / sum(n)) 

# compute the joint table 
tmp  <- pop_tab2$prop
prYX <- apply(popX %*% bbs, 1, function(x) exp(x) / sum(exp(x)))  
pr_joint <- data.frame(t(sapply(1:length(tmp), function(i) prYX[,i] * tmp[i])))
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")
pop_tab2 <- bind_cols(pop_tab2, pr_joint) %>% 
  select(-n, -prop) %>% 
  tidyr::pivot_longer(cols = c(`HS or Less`, `Some College`, `4-Year`, `Post-Grad`), 
                      names_to = "educ", values_to = "weights")

# estimate with the estimated weigthts
weight_cmlogit <- cces_nc %>% group_by(educ, age, gender) %>% 
  summarise(turnout = sum(vv_turnout) / n(), .groups = "drop") %>%
  left_join(pop_tab2, by = c("educ", "age", "gender")) %>% 
  mutate(scale_weight = weights / sum(weights)) 
turnout_cmlogit <- weight_cmlogit$scale_weight %*% weight_cmlogit$turnout

## emlogit 
pop_tab3 <- acs_nc %>% count(age, gender, wt = N) %>%  mutate(prop = n / sum(n)) 

# compute the joint table 
tmp  <- pop_tab3$prop
prYX <- apply(popX %*% bb0, 1, function(x) exp(x) / sum(exp(x)))  
pr_joint <- data.frame(t(sapply(1:length(tmp), function(i) prYX[,i] * tmp[i])))
colnames(pr_joint) <- c("HS or Less", "Some College", "4-Year", "Post-Grad")
pop_tab3 <- bind_cols(pop_tab3, pr_joint) %>% 
  select(-n, -prop) %>% 
  tidyr::pivot_longer(cols = c(`HS or Less`, `Some College`, `4-Year`, `Post-Grad`), 
                      names_to = "educ", values_to = "weights")

# estimate with the estimated weights
weight_emlogit <- cces_nc %>% group_by(educ, age, gender) %>% 
  summarise(turnout = sum(vv_turnout) / n(), .groups = "drop") %>%
  left_join(pop_tab3, by = c("educ", "age", "gender")) %>% 
  mutate(scale_weight = weights / sum(weights)) 
turnout_emlogit <- weight_emlogit$scale_weight %*% weight_emlogit$turnout



## results
est <- c(turnout_naive, turnout_nonpar, 
         turnout_cmlogit, turnout_emlogit)
estimator  <- c("Sample Mean", "Post-str. w/ True Joint",
                "Post-str. w/ cmlogit Joint",
                "Post-str. w/ emlogit Joint")
knitr::kable(
  cbind(estimator, round(est, 3), round(turnout, 3)),
  col.names = c("Estimator", "Estimate", "Truth"),
  digits = 3
)
```

| Estimator                  | Estimate | Truth |
| :------------------------- | :------- | :---- |
| Sample Mean                | 0.575    | 0.596 |
| Post-str. w/ True Joint    | 0.56     | 0.596 |
| Post-str. w/ cmlogit Joint | 0.559    | 0.596 |
| Post-str. w/ emlogit Joint | 0.569    | 0.596 |
