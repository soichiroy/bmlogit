

#' Constrained Multinominal Logistic Regression
#' @import nloptr
#' @export
cmlogit <- function(Y, X, popY, popX, popN, control = list()) {

  ## obtain the initial value via emlogit (multinominal logit without constraints )
  init <- emlogit::emlogit(Y, X, control)

  ## setting
  opts <- - list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1.0e-4)

  ## data
  n_item <- length(unique(Y))
  n_var  <- ncol(X)
  dat_list <- list(Y = Y, X = X, popY = popY, popX = popX, popN = popN,
                   n_item = n_item, n_var = n_var)

  ## fit
  fit <- nloptr(
    x0            = as.vector(init),
    eval_f        = fn_ll,
    # eval_grad_f   = fn_ll_grad,
    eval_g_eq     = fn_ct,
    # eval_jac_g_eq = fn_ct_jac
    dat_list      = dat_list
  )
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
  bmat <- cbind(0, matrix(x, nrow = ncol(X), ncol = n_item-1))

  ## compute Xβ - n by J
  Xb <- X %*% bmat

  ## log-likelihood
  ll_vec <- rowSums(Y * XB) - apply(Xb, 1, log_sum_exp)
  return(-sum(ll_vec))
}


#' Gradient of the log-likelihood function
#' @param x A vector of parameters
#' @keywords internal
fn_ll_grad <- function(x, dat_list) {

}


#' Evaluate Constraints
#' @param x A vector of parameters
#' @keywords internal
fn_ct <- function(x, dat_list) {

  ## obtain data
  popY <- dat_list$popY
  popX <- dat_list$popX
  popN <- dat_list$popN
  n_item <- dat_list$n_item

  ##
  ## constraints:
  ##
  ##  Pr(Y = j) = E[exp(Xβ[j]) / sum(exp(Xβ[j']))]
  ##
  ## where the expectation is over X ~ p(X)
  ##

  bmat <- cbind(0, matrix(x, nrow = ncol(X), ncol = n_item-1))
  Xpb  <- popX %*% bmat

  ## Pr(Y = j | X) -- n by J
  prYX  <- apply(Xpb, 1, function(x) exp(x) / sum(exp(x)))

  ## Pr(Y = j) = E[Pr(Y = j | X)]
  prYj  <- as.vector(t(prY) %*% (popN / sum(popN)))

  ## compute the loss
  loss <- popY - prYj
  return(loss)
}
