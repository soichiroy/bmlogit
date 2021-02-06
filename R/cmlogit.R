

#' Constrained Multinominal Logistic Regression
#' @import nloptr
#' @export
cmlogit <- function(Y, X, popY, popX, popN, control = list()) {

  ## obtain the initial value via emlogit (multinominal logit without constraints )
  init <- emlogit::emlogit(Y, X, control)
  init_coef <- init$coef[,-1]

  ## setting
  local_opts <- list("algorithm" = "NLOPT_LN_COBYLA",
                     "xtol_rel"=1.0e-8)
  opts <- list("algorithm" = "NLOPT_LN_AUGLAG", "xtol_rel" = 1.0e-8,
               "local_opts" = local_opts, "maxeval" = 2000)

  ## data
  n_item <- ncol(Y)
  n_var  <- ncol(X)
  dat_list <- list(Y = Y, X = X, popY = popY, popX = popX, popN = popN,
                   n_item = n_item, n_var = n_var, ep = control$tol_pred)

  ## fit
  fit <- nloptr(
    x0            = as.vector(init_coef),
    eval_f        = fn_ll,
    # eval_grad_f   = fn_ll_grad,
    # eval_g_eq     = fn_ct,
    # eval_jac_g_eq = fn_ct_jac
    eval_g_ineq   = fn_ct,
    opts          = opts,
    dat_list      = dat_list
  )

  ## coef
  coef_est <- cbind(0, matrix(fit$solution, nrow = ncol(X)))

  return(list(fit = fit, init = init, coef = coef_est))
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
  ll_vec <- rowSums(Y * Xb) - apply(Xb, 1, log_sum_exp)
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
  bmat <- cbind(0, matrix(x, nrow = ncol(X), ncol = n_item-1))

  ## Pr(Y = j | X) -- n by J
  prYX  <- apply(popX %*% bmat, 1, function(x) exp(x) / sum(exp(x)))

  ## Pr(Y = j) = E[Pr(Y = j | X)]
  prYj  <- as.vector(prYX %*% (popN / sum(popN)))

  ## compute the loss
  loss <- sum(abs(popY - prYj)) - ep
  return(loss)
}
