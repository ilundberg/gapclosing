
#' Ridge regression estimator
#' @description Not typically called by the user directly; called indirectly via other functions. Uses \code{glmnet} to fit a ridge regression with penalty chosen by cross-validation. Returns fitted values for the data in \code{to_predict}.
#' @param data Data frame containing the observed data
#' @param model_formula A model formula object for the ridge regression to be fitted
#' @param to_predict Data frame containing observations for which predictions are to be made. If NULL, defaults to the same as \code{data}.
#' @return A list containing a \code{glmnet} model object \code{fit} and a vector \code{fitted} of fitted values for observations in \code{to_predict}.
#' @references Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
#' @references Friedman J, Hastie T, Tibshirani R (2010). "Regularization Paths for Generalized Linear Models via Coordinate Descent." Journal of Statistical Software, 33(1), 1–22. https://www.jstatsoft.org/v33/i01/.
#' @export

fit_ridge <- function(data, model_formula, to_predict) {
  if (is.null(data$gapclosing.weight)) {
    stop("ERROR: fit_ridge requires that data contains a variable named weight. The package should have created this internally in a higher function.")
  }
  outcome <- data[[as.character(model_formula)[2]]]
  X <- stats::model.matrix(model_formula, data = data)
  X_predict <- stats::model.matrix(model_formula, data = to_predict, alpha = 0)
  if (!all(colnames(X) == colnames(X_predict))) {
    stop("Error: Within ridge estimation, the model matrices for learning and prediction have different column names")
  }
  fit <- glmnet::cv.glmnet(x = X, y = outcome,
                           family = ifelse(all(outcome %in% 0:1), "binomial", "gaussian"),
                           weights = data[["weight"]])
  fitted <- stats::predict(fit, newx = X_predict, type = "response", s = fit$lambda.min)
  return(list(fitted = fitted[,1], fit = fit))
}
