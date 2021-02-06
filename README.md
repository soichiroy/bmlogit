
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

This is a basic example which shows you how to solve a common problem:

``` r
library(cmlogit)
library(synthArea)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
#> ✔ ggplot2 3.3.2     ✔ purrr   0.3.4
#> ✔ tibble  3.0.4     ✔ stringr 1.4.0
#> ✔ tidyr   1.1.2     ✔ forcats 0.5.0
#> ✔ readr   1.3.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()


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
               control = list(intercept = FALSE))


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
