
#' Point estimator for gap-closing estimands
#' @description This is an internal function typically called from other functions rather than by the user. It uses a learning sample to learn the nuisance functions (treatment and outcome model) and then an auxiliary estimation sample to use those functions in estimation of the gap-closing estimand. For single-sample estimation, both the learning and estimation samples are the same. For cross-fitting, this function is called repeatedly with the roles of each dataset swapped.
#' @param data_learn Data frame in which treatment and outcome models will be learned
#' @param data_estimate Data frame in which the learned models will be converted to an estimate of the gap-closing estimand
#' @param counterfactual_assignments Numeric scalar or vector of length nrow(data), each element of which is on the \[0,1\] interval. If a scalar, the counterfactual probability by which all units are assigned to treatment condition 1. If a vector, each element i corresponds to the counterfactual probability by which each unit i is assigned to treatment condition 1.
#' @param outcome_formula Model formula the outcome. Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph). If \code{outcome_algorithm} = "ranger", then the outcome model will be fit separately on the treatment and control groups. Otherwise, the user must specify all interactions in the formula.
#' @param treatment_formula Treatment formula, in the style formula(treatment ~ covariates). Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph).
#' @param category_name Character name of the variable indicating the categories over which the gap is defined. Must be the name of a column in \code{data}.
#' @param outcome_name Character name of the outcome variable. Only required when there is no outcome_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_name Character name of the treatment variable. Only required when there is no treatment_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_algorithm Character name of the algorithm for the treatment model. One of "glm", "ridge", "gam", or "ranger". Defaults to "glm", which is a logit model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}). If "ranger", this function avoids propensity scores equal to 0 or 1 by bottom- and top-coding predicted values at .001 and .999.
#' @param outcome_algorithm Character name of the algorithm for the outcome model. One of "lm", "ridge", "gam", or "ranger". Defaults to "lm", which is an OLS model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}).
#' @param weight_name Character name of a sampling weight variable, if any, which captures the inverse probability of inclusion in the sample. The default assumes a simple random sample (all weights equal).
#' @return @return A list with four elements.
#' \itemize{
#' \code{counterfactual_means} A data frame with four columns
#' \itemize{
#' \item\code{category} The category to which each estimate applies.
#' \item\code{treatment_modeling} Post-intervention mean estimates via treatment modeling. Estimates are NA if a treatment model was not provided.
#' \item\code{outcome_modeling} Post-intervention mean estimates via outcome modeling. Estimates are NA if an outcome model was not provided.
#' \item\code{doubly_robust} Post-intervention mean estimates via doubly-robust estimation. Estimate are NA if either a treatment or an outcome model was not provided.
#' }
#' \item\code{treatment_model} Object containing the fitted treatment model.
#' \item\code{outcome_model} Object containing the fitted outcome model.
#' \item\code{estimation_weights} \code{estimation_weights} If \code{sample_split} = "single_sample", a numeric vector of length \code{nrow(data)}. Within categories, the weighted average of the outcome with these weights is the treatment modeling estimate of the post-intervention mean defined by \code{counterfactual_assignments}.
#' }
#' A data frame with four columns. \code{category} contains the category to which each estimate applies. \code{treatment_modeling}, \code{outcome_modeling}, and \code{doubly_robust} contain the post-intervention mean estimate for that category, by each approach. When an approach is not applicable (because \code{treatment_formula} or \code{outcome_formula} was not provided), the corresponding columns of estimates are NA.
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @export

point_estimator <- function(
  data_learn,
  data_estimate,
  counterfactual_assignments,
  outcome_formula,
  treatment_formula,
  category_name,
  outcome_name,
  treatment_name,
  treatment_algorithm = "glm",
  outcome_algorithm = "lm",
  weight_name = NA
) {
  # Initialize non-standard evaluation variables to avoid R CMD check warnings.
  weight <- NULL
  # Assign a weight variable
  if (is.na(weight_name)) {
    data_learn$weight <- rep(1,nrow(data_learn))
    data_estimate$weight <- rep(1,nrow(data_estimate))
  } else {
    data_learn$weight <- data_learn[[weight_name]]
    data_estimate$weight <- data_estimate[[weight_name]]
  }
  # If the user passed a scalar counterfactual assignment, convert to a vector
  if (length(counterfactual_assignments) == 1) {
    counterfactual_assignments <- rep(counterfactual_assignments, nrow(data_estimate))
  }

  # Fit the treatment model
  if (is.null(treatment_formula)) {
    fit_m <- NULL
    m_fitted <- rep(NA, nrow(data_estimate))
  } else if (treatment_algorithm == "glm") {
    fit_m <- stats::glm(treatment_formula,
                        data = data_learn,
                        family = stats::binomial,
                        weights = weight)
    # Extract fitted propensity score
    m_fitted <- stats::predict(fit_m, newdata = data_estimate, type = "response")
  } else if (treatment_algorithm == "ridge") {
    m_fitted <- fit_ridge(data = data_learn, model_formula = treatment_formula, to_predict = data_estimate)
  } else if (treatment_algorithm == "gam") {
    fit_m <- mgcv::gam(treatment_formula,
                       data = data_learn,
                       family = stats::binomial,
                       weights = weight)
    # Extract fitted propensity score
    m_fitted <- mgcv::predict.gam(fit_m, newdata = data_estimate, type = "response")
  } else if (treatment_algorithm == "ranger") {
    fit_m <- ranger::ranger(treatment_formula,
                            data = data_learn,
                            case.weights = data_learn$weight)
    m_fitted <- stats::predict(fit_m, data = data_estimate)$predictions
    m_fitted <- ifelse(m_fitted < .001, .001, m_fitted)
    m_fitted <- ifelse(m_fitted > .999, .999, m_fitted)
  }
  # Fit the outcome model
  data_estimate_1 <- data_estimate_0 <- data_estimate
  data_estimate_1[[treatment_name]] <- 1
  data_estimate_0[[treatment_name]] <- 0
  if (is.null(outcome_formula)) {
    fit_g <- NULL
    yhat1 <- yhat0 <- residual <- rep(NA, nrow(data_estimate))
  } else if (outcome_algorithm == "lm") {
    fit_g <- stats::lm(outcome_formula,
                       data = data_learn,
                       weights = weight)
    yhat1 <- stats::predict(fit_g, newdata = data_estimate_1)
    yhat0 <- stats::predict(fit_g, newdata = data_estimate_0)
    residual <- data_estimate[[outcome_name]] - stats::predict(fit_g, newdata = data_estimate)
  } else if (outcome_algorithm == "ridge") {
    yhat1 <- fit_ridge(data = data_learn, model_formula = outcome_formula, to_predict = data_estimate_1)
    yhat0 <- fit_ridge(data = data_learn, model_formula = outcome_formula, to_predict = data_estimate_0)
    residual <- data_estimate[[outcome_name]] - fit_ridge(data = data_learn, model_formula = outcome_formula, to_predict = data_estimate)
  } else if (outcome_algorithm == "gam") {
    fit_g <- mgcv::gam(outcome_formula,
                       data = data_learn,
                       weights = weight)
    yhat1 <- stats::predict(fit_g, newdata = data_estimate_1)
    yhat0 <- stats::predict(fit_g, newdata = data_estimate_0)
    residual <- data_estimate[[outcome_name]] - stats::predict(fit_g, newdata = data_estimate)
  } else if (outcome_algorithm == "ranger") {
    data_learn_1 <- data_learn[data_learn[[treatment_name]] == 1,]
    data_learn_0 <- data_learn[data_learn[[treatment_name]] == 0,]
    fit_g_1 <- ranger::ranger(outcome_formula,
                              data = data_learn,
                              case.weights = data_learn$weight)
    fit_g_0 <- ranger::ranger(outcome_formula,
                              data = data_learn,
                              case.weights = data_learn$weight)
    fit_g <- list(fit_g_1 = fit_g_1, fit_g_0 = fit_g_0)
    yhat1 <- stats::predict(fit_g_1, data = data_estimate)$predictions
    yhat0 <- stats::predict(fit_g_0, data = data_estimate)$predictions
    residual <- data_estimate[[outcome_name]] - ifelse(data_estimate[[treatment_name]] == 1,
                                                       stats::predict(fit_g_1, data = data_estimate)$predictions,
                                                       stats::predict(fit_g_0, data = data_estimate)$predictions)
  }

  # Make  the weight specific to each unit's factual treatment condition
  m_i <- ifelse(data_estimate[[treatment_name]] == 1, m_fitted, 1 - m_fitted)
  pi_i <- ifelse(data_estimate[[treatment_name]] == 1, counterfactual_assignments, 1 - counterfactual_assignments)
  w_i <- data_estimate$weight * pi_i  / m_i

  # Calculate estimates
  category_values <- as.character(unique(c(as.character(data_learn[[category_name]]),
                                           as.character(data_estimate[[category_name]]))))
  category_values <- sort(category_values)
  estimate <- matrix(NA, nrow = length(category_values), ncol = 3)
  colnames(estimate) <- c("treatment_modeling", "outcome_modeling", "doubly_robust")
  rownames(estimate) <- category_values
  for (category_value in category_values) {
    category_indices <- data_estimate[[category_name]] == category_value
    estimate[category_value,"outcome_modeling"] <- stats::weighted.mean(
      counterfactual_assignments[category_indices] * yhat1[category_indices] +
        (1 - counterfactual_assignments[category_indices]) * yhat0[category_indices],
      w = data_estimate$weight[category_indices]
    )
    estimate[category_value,"treatment_modeling"] <- stats::weighted.mean(
      data_estimate[[outcome_name]][category_indices],
      w = w_i[category_indices]
    )
    augmentation <- stats::weighted.mean(-residual[category_indices], w = w_i[category_indices])
    estimate[category_value,"doubly_robust"] <- stats::weighted.mean(
      estimate[category_value,"outcome_modeling"] - augmentation
    )
  }

  object_to_return <- list(counterfactual_means = estimate,
                           treatment_model = fit_m,
                           outcome_model = fit_g,
                           estimation_weights = w_i)

  return(object_to_return)
}

