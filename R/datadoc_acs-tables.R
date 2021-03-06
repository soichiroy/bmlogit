#' Three-way ACS Tables for Georgia
#'
#'
#' Table of counts by CD from the 2018 one-year ACS in Georgia. We load the
#' variables that roughly partition gender, age, and race. We get another
#' three way table of gender, age, and education. That is, this is the
#' output of `get_acs_cces(acscodes_age_sex_race)` and `get_acs_cces(acscodes_age_sex_educ)`.
#'
#' @rdname acs_GA
#' @source
#' Kyle Walker and Matt Herman (2021). tidycensus: Load US Census Boundary and Attribute Data as
#'  'tidyverse' and 'sf'-Ready Data Frames. R package version 0.11.4.
#'  <https://CRAN.R-project.org/package=tidycensus>
#'
#' @examples
#'  library(tibble)
#'  acs_race_GA
#'  acs_educ_GA
#'
"acs_educ_GA"

#' @rdname acs_GA
"acs_race_GA"
