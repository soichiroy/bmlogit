

#' Constrained Multinominal Logistic Regression
#'
#' @param Y A matrix of binary outcomes where each columns represents a level of the outcome
#' @param X A model matrix of covariates. An intercept must be included.
#' @param target_Y A vector of proportions that the final probabilities should
#'  align to (with tolerance set in the argument \code{tol_pred} in control). This should
#'  have the same number of elements as the columns of `Y`.
#' @param pop_X A table of the unique combinations of the `X` variables.  Together
#'  with `count_X`, this represents the traditional population table to post-stratify on.
#' @param count_X A vector of population counts for each of   the possible combinations
#'  of `X`.  Values must be ordered to be the same as the rows of `pop_X`.
#' @return The function returns a list of class \code{cmlogit} object.
#' @import nloptr
#' @export
cmlogit <- function(Y, X, target_Y, pop_X, count_N, control = list()) {

  control <- input_check(control)

  ## obtain the initial value via emlogit (multinominal logit without constraints )
  init <- emlogit::emlogit(Y, X, control)
  init_coef <- init$coef[,-1]

  X <- as.matrix(X)
  # control <- input_check(control)
  if (isTRUE(control$intercept)) {
    X <- cbind(1, X)
    pop_X <- cbind(1, pop_X)
  }


  ## setting
  local_opts <- list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel"  = 1.0e-8)
  opts <- list("algorithm" = "NLOPT_LN_AUGLAG", "xtol_rel" = 1.0e-8,
               "local_opts" = local_opts, "maxeval" = 2000)

  ## data
  n_item <- ncol(Y)
  n_var  <- ncol(X)
  dat_list <- list(Y = Y, X = X, target_Y = target_Y, pop_X = pop_X, count_N = count_N,
                   n_item = n_item, n_var = n_var, ep = control$tol_pred)

  ## fit
  fit <- nloptr(
    x0            = as.vector(init_coef),
    eval_f        = fn_ll,
    eval_g_ineq   = fn_ct,
    opts          = opts,
    dat_list      = dat_list
  )

  ## coef
  coef_est  <- cbind(0, matrix(fit$solution, nrow = n_var, ncol = n_item - 1))
  coef_init <- init$coef
  ## predict
  prob <- predict_prob(X, coef_est)

  out <- list(coef = coef_est, fitted = prob, coef_init = coef_init, control = control)
  class(out) <- c("cmlogit", "cmlogit.est")
  return(out)
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
  ll_vec <- rowSums(Y * Xb) - apply(Xb, 1, log_sum_exp)
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
  count_N <- dat_list$count_N
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
  prYj  <- as.vector(prYX %*% (count_N / sum(count_N)))

  ## compute the loss
  loss <- sum(abs(target_Y - prYj)) - ep
  return(loss)
}




#' Input check
#' A function to set the default values of \code{control}.
#' @param control A list of control parameters.
#' @keywords internal
input_check <- function(control) {
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
