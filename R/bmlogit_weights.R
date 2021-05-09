
#' Entroply balancing weights for survey
#' @param x_survey A matrix of survey respondents
#' @param x_target A vector of population mean (or higher moments).
#'                 Length of \code{x_target} should match
#'                  the number of columns in \code{x_survey}.
#' @return A vector of normalized weights.
#' @keywords internal
ebal_survey <- function(x_survey, x_target, add_intercept = TRUE) {

  ## ebal objective function
  ebal_survey_obj <- function(par, x_survey, x_target) {
    return(log(sum(exp(x_survey %*% par))) - x_target %*% par)
  }

  ## add intercept
  if (isTRUE(add_intercept)) {
    x_survey <- cbind(rep(1, nrow(x_survey)), x_survey)
    x_target <- c(1, x_target)
  }

  ## obtain info about the input
  par_init <- rnorm(length(x_target))

  ## estimate parameters
  fit <- optim(par = par_init, fn = ebal_survey_obj,
                x_survey = x_survey, x_target = x_target,
                method = "BFGS"
              )

  ## estimate weights
  weights <- exp(x_survey %*% fit$par)
  weights <- weights / sum(weights)
  return(as.vector(weights))
}


#' Covariate Balancing Propensity Score
#' @keywords internal
#' @inheritParams ebal_survey
#' @param n_pop Population size.
#' @return A vector of normalized weights.
cbsw_survey <- function(x_survey, x_target, n_pop,  add_intercept = TRUE) {

  ## define loss functions
  cbsw_loss <- function(par, x_survey, x_target, n_pop) {
    B <- x_target %*% par
    A <- x_survey %*% par - exp(-x_survey %*% par)
    loss <- sum(A)/n_pop - B
    return(-loss)
  }

  ## add intercept
  if (isTRUE(add_intercept)) {
    x_survey <- cbind(rep(1, nrow(x_survey)), x_survey)
    x_target <- c(1, x_target)
  }

  ## solve for parametrs
  convergence <- 1; max_trial <- 20; iter <- 1
  while(iter <= max_trial   & convergence != 0) {
    par_init <- rnorm(length(x_target))
    fit <- optim(
      par      = par_init,
      fn       = cbsw_loss,
      gr       = NULL,
      x_survey = x_survey,
      x_target = x_target,
      n_pop    = n_pop,
      method   = 'BFGS'
    )
    convergence <- fit$convergence
    iter <- iter + 1
  }

  ## prob
  pr <- 1 / (1 + exp(-x_survey %*% fit$par))

  ## weights
  weights <- (1 / pr) / sum(1 / pr)
  return(list(coef = fit$par, prob = pr, w = weights))
}




cbsw_grad <- function(par, x_survey, x_target, n_pop) {
  w <- as.vector(1 + exp(-x_survey %*% par))
  loss <- x_target - as.vector(apply(x_survey, 2, function(x) sum(x * w) / n_pop))
  return(loss)
}
