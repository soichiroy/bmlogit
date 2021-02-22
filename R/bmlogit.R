

#' Constrained Multinominal Logistic Regression
#'
#' @param Y A matrix of binary outcomes where each columns represents a level of the outcome
#' @param X A model matrix of covariates. The recommended setup is to _not_ include
#'  a intercept (and keep the `intercept = TRUE` default in controls), and
#'  remove a baseline level in the categorical variables.
#' @param target_Y A vector of proportions that the final probabilities should
#'  align to (with tolerance set in the argument \code{tol_pred} in control). This should
#'  have the same number of elements as the columns of `Y`.
#' @param pop_X A table of the unique combinations of the `X` variables.  Together
#'  with `count_X`, this represents the traditional population table to post-stratify on.
#' @param count_X A vector of population counts for each of   the possible combinations
#'  of `X`.  Values must be ordered to be the same as the rows of `pop_X`.
#' @inheritParams emlogit::emlogit
#' @return The function returns a list of class \code{bmlogit} object.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' ## survey data
#' Y <- model.matrix(~ educ - 1, data = cc18_GA)
#' colnames(Y) <- levels(cc18_GA$educ)
#' X <- model.matrix(~ age + female + race, data = cc18_GA)[, -1]
#'
#' ## population table
#' pop_X_df <- count(acs_race_GA, age, female, race, wt = count, name = "count")
#' pop_X    <- model.matrix(~ age + female + race, data = pop_X_df)[, -1]
#' count_X  <- pull(pop_X_df, count)
#'
#' ## population target
#' edu_tgt  <- count(acs_educ_GA, educ, wt = count, name = "count") %>%
#'   deframe()
#'
#' ## fit
#' fit <- bmlogit(
#'   Y = Y,
#'   X = X,
#'   pop_X    = pop_X,
#'   count_X  = count_X,
#'   target_Y = edu_tgt,
#'   control  = list(tol_pred = 0.05))
#'
#'
bmlogit <- function(Y, X, target_Y, pop_X, count_X, control = list()) {

  ## set control options
  control <- set_control_default(control)

  # check input
  input_check(Y, X, target_Y, pop_X, count_X, control)

  ## bmlogit_run
  coef_est <- bmlogit_run(Y, X, target_Y, pop_X, count_X, control)

  ## predict
  # Add again (operation in bmlogit_run does not get carried over)
  if (isTRUE(control$intercept))
    X <- cbind(1, X)
  prob <- predict_prob(X, coef_est)

  fit <- list(coef = coef_est,
              fitted = prob,
              control = control,
              x_name = colnames(X),
              y_name = colnames(Y))
  class(fit) <- c("bmlogit", "bmlogit.est")
  return(fit)
}

# temporary dual use
cmlogit <- bmlogit


#' Internal core function
#' @import nloptr
#' @importFrom emlogit emlogit
#' @keywords internal
bmlogit_run <- function(Y, X, target_Y, pop_X, count_X, control) {
  ## obtain the initial value via emlogit (multinominal logit without constraints )
  init <- emlogit(Y, X, control)
  init_coef <- init$coef[,-1]

  ## add intercept term if necessary
  X <- as.matrix(X)
  if (isTRUE(control$intercept)) {
    X <- cbind(1, X)
    pop_X <- cbind(1, pop_X)
  }

  ## setting nloptr options
  local_opts <- list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel"  = 1.0e-8)
  opts <- list("algorithm" = "NLOPT_LN_AUGLAG", "xtol_rel" = 1.0e-8,
               "local_opts" = local_opts, "maxeval" = 2000)

  ## data
  n_item <- ncol(Y)
  n_var  <- ncol(X)
  dat_list <- list(
    Y = Y,
    X = X,
    target_Y = target_Y,
    pop_X = pop_X,
    count_X = count_X,
    n_item = n_item,
    n_var = n_var,
    ep = control$tol_pred
    )

  ## fit
  fit <- nloptr(
    x0            = as.vector(init_coef),
    eval_f        = fn_ll,
    eval_g_ineq   = fn_ct,
    opts          = opts,
    dat_list      = dat_list
  )

  ## coef
  coef  <- cbind(0, matrix(fit$solution, nrow = n_var, ncol = n_item - 1))
  return(coef)
}

predict_prob <- function(X, coef) {
  Xb <- X %*% coef
  pr <- t(apply(Xb, 1, function(x) exp(x) / sum(exp(x))))
  return(pr)
}

# compute log(exp(x[1]) + ... + exp(x[n]))
log_sum_exp <- function(x) {
  x_max <- max(x)
  out <- x_max + log(sum(exp(x - x_max)))
  return(out)
}


#' Log-likelihood function
#' @param x A vector of parameters
#' @param Y A matrix of choice (n by J)
#' @keywords internal
fn_ll <- function(x, dat_list) {

  Y <- dat_list$Y
  X <- dat_list$X
  n_item <- dat_list$n_item

  ## bmat = [beta[1] = 0,... beta[j], ..., beta[J]]
  bmat <- cbind(0, matrix(x, nrow = dat_list$n_var, ncol = n_item-1))

  ## compute Xβ - n by J
  Xb <- X %*% bmat

  ## log-likelihood
  ll_vec <- rowSums(Y * Xb) - apply(Xb, 1, log_sum_exp) # + sum(dnorm(x, mean = 0, sd = 5, log = TRUE))
  return(-sum(ll_vec))
}


#' Gradient of the log-likelihood function
#' @param x A vector of parameters
#' @keywords internal
fn_ll_grad <- function(x, dat_list) {
  Y <- dat_list$Y
  X <- dat_list$X
  n_item <- dat_list$n_item

  ## bmat = [beta[1] = 0,... beta[j], ..., beta[J]]
  bmat <- cbind(0, matrix(x, nrow = dat_list$n_var, ncol = n_item-1))
  resid <- Y - t(apply(X %*% bmat, 1, function(x) exp(x) / sum(exp(x))))
  score <- t(resid) %*% X
  grad <- as.vector(t(score)[,-1])
  return(grad)
}


#' Evaluate Constraints
#' @param x A vector of parameters
#' @keywords internal
fn_ct <- function(x, dat_list) {


  ## obtain data
  target_Y <- dat_list$target_Y
  pop_X <- dat_list$pop_X
  count_X <- dat_list$count_X
  n_item <- dat_list$n_item
  ep     <- dat_list$ep

  ##
  ## constraints:
  ##
  ##  Pr(Y = j) = E[exp(Xβ[j]) / sum(exp(Xβ[j']))]
  ##
  ## where the expectation is over X ~ p(X)
  ##
  ## We replace the above hard constraints with
  ##
  ## || Pr(Y = j) = E[exp(Xβ[j]) / sum(exp(Xβ[j']))] || <= ϵ
  ##
  ## where ϵ is set to a small constant
  ##
  bmat <- cbind(0, matrix(x, nrow = dat_list$n_var, ncol = n_item-1))

  ## Pr(Y = j | X) -- n by J
  prYX  <- apply(pop_X %*% bmat, 1, function(x) exp(x) / sum(exp(x)))

  ## Pr(Y = j) = E[Pr(Y = j | X)]
  prYj  <- as.vector(prYX %*% (count_X / sum(count_X)))

  ## compute the loss
  loss <- sum(abs(target_Y - prYj)) - ep
  return(loss)
}




#' Set control default
#' A function to set the default values of \code{control}.
#' @param control A list of control parameters.
#' @keywords internal
set_control_default <- function(control) {
  if (!exists("max_iter", control)) {
    control$max_iter <- 200
  }

  if (!exists("tol", control)) {
    control$tol <- 1e-6
  }

  if (!exists("tol_pred", control)) {
    control$tol_pred <- 0.01
  }


  if (!exists("verbose", control)) {
    control$verbose = FALSE
  }

  if (!exists("intercept", control)) {
    control$intercept <- TRUE
  }

  if (!exists("robust", control)) {
    control$robust <- FALSE
  }

  if (!exists("variance", control)) {
    control$variance <- TRUE
  }

  if (!exists("initialize", control)) {
    control$initialize <- "ls"
  } else {
    if (!(control$initialize %in% c("ls", "random"))) {
      stop("Not a supported initialization method")
    }
  }

  return(control)
}

#' Check dimensions of inputs
#' @inheritParams bmlogit
#' @importFrom checkmate assert_matrix assert_vector assert_set_equal
#' @keywords internal
input_check <- function(Y, X, target_Y, pop_X, count_X, control) {
  # Variable class ----
  assert_matrix(Y)
  assert_matrix(X)
  assert_matrix(pop_X)
  assert_vector(target_Y)
  assert_vector(count_X)

  # dimensions agree --
  assert_set_equal(NCOL(Y), length(target_Y))
  assert_set_equal(NCOL(X), NCOL(pop_X))
  assert_set_equal(NROW(pop_X), length(count_X))

  # target levels agree
  if (!is.null(colnames(Y)) & !is.null(names(target_Y))) {
    assert_set_equal(colnames(Y), names(target_Y), ordered = TRUE)
  }
}

