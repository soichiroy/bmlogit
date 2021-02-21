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
#'   \item{count}{Estimated count of each cell. This is derived from `prop_electorate`
#'   multiplied by the total implied by `elec18_GA`.}
#' }
"catalist18_GA"
