#' CCES 2018 from Georgia
#'
#'
#' @format A survey dataset (n = `r nrow(cc18_GA)`) from the 2018 CCES
#' \describe{
#'   \item{year, case_id, ...}{Standard CCES variables. See https://doi.org/10.7910/DVN/II2DB6}
#'   \item{cd}{Congressional District}
#'   \item{voted_gov_party}{Post-Election Self-Report Vote for Governor}
#'   \item{intent_gov_party}{Pre-Election Self-Report Vote for Governor}
#'   \item{voted_rep_party}{Pre-Election Self-Report Vote for U.S. House}
#'   \item{vv_turnout_gvm}{Validated Vote for General Election}
#'   \item{voted_govR}{A numerical version of `voted_gov_party` where people
#'    who did not answer the question or said they did not vote are given
#'    a 0.5, Republican vote is given a 1 and Democratic vote is given a 0}
#'   \item{intent_govR}{A numerical version of `intent_gov_party` where people
#'    who did not answer the question or said they did not vote are given
#'    a 0.5, Republican vote is given a 1 and Democratic vote is given a 0}
#'   ...
#' }
"cc18_GA"


#' Georgia Election Results
#'
#'
#' @source http://bit.ly/2LNgqh5
#'
#' @format Election counts
#' \describe{
#'   \item{cd}{Congressional District},
#'   \item{abrams}{Votes for the Democrat, Stacey Abrams},
#'   \item{kemp}{Votes for the Republican, Brian Kemp},
#'   \item{total}{Total number of votes, including third party}
#' }
"elec18_GA"


#' Estimated Proportion of demographics in t
#' @source http://bit.ly/3aiAJN2
#'
#' @format Proportions
#' \describe{
#'   \item{race_age}{Race - age combination, which matches up with the variable
#'      of the same variable in `cc18_GA`}
#'   \item{prop_electorate}{Proportion of the turnout electorate, reported
#'     by Catalist (see Source link)}
#' }
"catalist18_GA"
