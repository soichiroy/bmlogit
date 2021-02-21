
#' Obtain the predicted probability
#' @param object An output of \code{bmlogit()} function.
#' @param newdata A matrix of newdata. If not provided, the insample fit is returned.
#' @param counts A vector of population counts corresponding to the rows of \code{newdata}.
#'  If provided \code{predict} function returns the predicted outcome at the aggregate level.
#' @param weights A vector of population weights corresponding to the rows of \code{newdata}.
#'  If provided \code{predict} function returns the predicted outcome at the aggregate level.
#'  If \code{counts} is provided, this argument will be ignored.
#' @param ... Unused parameter, added to be consistent with regular predict
#' @return A matrix (or a vector) of predicted probabilities.
#' @export
#'
#' @examples
#' library(dplyr)
#' ## survey data
#' Y <- model.matrix(~ educ - 1, data = cc18_GA)
#' X <- model.matrix(~ age + female + race, data = cc18_GA)[, -1]
#'
#' ## population table
#' pop_X_df <-  count(acs_race_GA, age, female, race, wt = count, name = "count")
#' pop_X    <- model.matrix(~ age + female + race, data = pop_X_df)[, -1]
#' count_X  <- pull(pop_X_df, count)
#'
#' ## population target
#' edu_tgt  <- count(acs_educ_GA, educ, wt = count, name = "count") %>%
#'   pull(count)
#'
#' ## fit
#' fit <- bmlogit(
#'   Y = Y,
#'   X = X,
#'   pop_X    = pop_X,
#'   count_X  = count_X,
#'   target_Y = edu_tgt,
#'   control  = list(intercept = FALSE, tol_pred = 0.05))
#'
#' pred <- predict(fit)
#' dim(pred)
#' head(pred)
#'
#' pred <- predict(fit, newdata = pop_X[1:3, ])
#' bind_cols(pop_X_df[1:3, ],
#'           pred)
#' pred <- predict(fit, newdata = pop_X, counts = count_X)
#' pred
#'
predict.bmlogit <- function(object, ..., newdata = NULL, counts = NULL, weights = NULL) {

  if (is.null(newdata)) {
    pred <- object$fitted
  } else {
    if (isTRUE(object$control$intercept)) {
      newdata <- cbind(rep(1, nrow(newdata)), newdata)
    }
    pred <- predict_prob(newdata, object$coef)
  }

  if (!is.null(object$y_name)) {
    colnames(pred) <- object$y_name
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
