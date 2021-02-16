
#' Obtain the predicted probability
#' @param obj An output of \code{bmlogit()} function.
#' @param newdata A matrix of newdata. If not provided, the insample fit is returned.
#' @param counts A vector of population counts corresponding to the rows of \code{newdata}.
#'  If provided \code{predict} function returns the predicted outcome at the aggregate level.
#' @param counts A vector of population weights corresponding to the rows of \code{newdata}.
#'  If provided \code{predict} function returns the predicted outcome at the aggregate level.
#'  If \code{counts} is provided, this argument will be ignored.
#' @return A matrix (or a vector) of predicted probabilities.
#' @export
predict.bmlogit <- function(obj, newdata = NULL, counts = NULL, weights = NULL) {

  if (is.null(newdata)) {
    pred <- obj$fitted
  } else {
    if (isTRUE(obj$control$intercept)) {
      newdata <- cbind(rep(1, nrow(newdata)), newdata)
    }
    pred <- predict_prob(newdata, obj$coef)
  }

  if (!is.null(obj$y_name)) {
    colnames(pred) <- obj$y_name
  }

  ## summarize the prediction by the population weights
  if (!is.null(counts)) {
    wt <- counts / sum(counts)
    pred <- as.vector(t(pred) %*% wt)
  } else if (is.null(counts) & !is.null(weights)) {
    pred <- as.vector(t(pred) %*% weights)
  }
  return(pred)
}
