
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
#' \code{counterfactual_means} A tibble with a counterfactual mean estimate for each category
#' \code{counterfactual_means} A tibble with a counterfactual disparity estimate for each pair of categories
#' \code{treatment_model} Object containing the fitted treatment model
#' \code{outcome_model} Object containing the fitted outcome model
#' }
#' @references Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
#' @importFrom foreach %do%
#' @importFrom magrittr %>%
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
  weight_name = NULL
) {
  # Initialize non-standard evaluation variables to avoid R CMD check warnings.
  gapclosing.weight <- gapclosing.treatment <- gapclosing.outcome <- gapclosing.counterfactual_assignments <-
    gapclosing.m_fitted <- gapclosing.pi_i <- gapclosing.m_i <- gapclosing.w_i <- gapclosing.yhat <-
    gapclosing.yhat1 <- gapclosing.yhat0 <- gapclosing.residual <- outcome_modeling <-
    gapclosing.robust_augmentation <- method <- method_case <- NULL
  # Assign a weight variable
  if (is.null(weight_name)) {
    data_learn$gapclosing.weight <- rep(1,nrow(data_learn))
    data_estimate$gapclosing.weight <- rep(1,nrow(data_estimate))
  } else {
    data_learn$gapclosing.weight <- data_learn[[weight_name]]
    data_estimate$gapclosing.weight <- data_estimate[[weight_name]]
  }
  # Enforce normalization of the weights
  data_learn$gapclosing.weight <-  data_learn$gapclosing.weight / mean(data_learn$gapclosing.weight, na.rm = TRUE)
  data_estimate$gapclosing.weight <-  data_estimate$gapclosing.weight / mean(data_estimate$gapclosing.weight, na.rm = TRUE)
  # Create easily-accessible treatment and outcome variables with known names,
  # as well as containing the counterfactual assignments
  data_learn <- data_learn %>%
    dplyr::mutate(gapclosing.treatment = data_learn[[treatment_name]],
                  gapclosing.outcome = data_learn[[outcome_name]],
                  gapclosing.counterfactual_assignments = NA)
  data_estimate <- data_estimate %>%
    dplyr::mutate(gapclosing.treatment = data_estimate[[treatment_name]],
                  gapclosing.outcome = data_estimate[[outcome_name]],
                  gapclosing.counterfactual_assignments = counterfactual_assignments)

  #################################################
  # Prepare datasets in which to make predictions #
  #################################################

  # Prepare data modified to take each counterfactual treatment value,
  # to be used to predict potential outcomes
  data_estimate_1 <- data_estimate_0 <- data_estimate
  data_estimate_1[[treatment_name]] <- 1
  data_estimate_0[[treatment_name]] <- 0

  #######################################################
  # Predict the treatment probability given confounders #
  #######################################################
  if (is.null(treatment_formula)) {
    fit_m <- NULL
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.m_fitted = NA)
  } else if (treatment_algorithm == "glm") {
    # The withCallingHandlers turns off the warning for non-integer number of successes,
    # since we expect that warning when there are weights.
    fit_m <- withCallingHandlers({
      stats::glm(treatment_formula,
                 data = data_learn,
                 family = stats::binomial,
                 weights = gapclosing.weight)
    }, warning = function(w) {
      if (grepl("non-integer",conditionMessage(w)))
        invokeRestart("muffleWarning")
    })
    # Extract fitted propensity score
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.m_fitted = stats::predict(fit_m, newdata = data_estimate, type = "response"))
  } else if (treatment_algorithm == "ridge") {
    fit_ridge.out <- fit_ridge(data = data_learn,
                               model_formula = treatment_formula,
                               to_predict = data_estimate)
    fit_m <- fit_ridge.out$fit
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.m_fitted = fit_ridge.out$fitted)
  } else if (treatment_algorithm == "gam") {
    fit_m <- withCallingHandlers({
      mgcv::gam(treatment_formula,
                data = data_learn,
                family = stats::binomial,
                weights = gapclosing.weight)
    }, warning = function(w) {
      if (grepl("non-integer",conditionMessage(w)))
        invokeRestart("muffleWarning")
    })
    # Extract fitted propensity score
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.m_fitted = mgcv::predict.gam(fit_m, newdata = data_estimate, type = "response"))
  } else if (treatment_algorithm == "ranger") {
    fit_m <- ranger::ranger(treatment_formula,
                            data = data_learn,
                            case.weights = data_learn$gapclosing.weight)
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.m_fitted = stats::predict(fit_m, data = data_estimate)$predictions,
                    # Truncate the fitted propensity score because random forest can
                    # produce fits of exactly 0 and 1, which create weighting problems
                    gapclosing.m_fitted = dplyr::case_when(gapclosing.m_fitted < .001 ~ .001,
                                                           gapclosing.m_fitted <= .999 ~ gapclosing.m_fitted,
                                                           gapclosing.m_fitted > .999 ~ .999))
    # To keep data sets in parallel to each other, make NA for that variable in data_learn
    data_learn$gapclosing.m_fitted <- NA
  }

  ############################################################
  # Predict the outcome mean given treatment and confounders #
  ############################################################

  if (is.null(outcome_formula)) {
    fit_g <- NULL
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.yhat1 = NA,
                    gapclosing.yhat0 = NA,
                    gapclosing.yhat = NA,
                    gapclosing.residual = NA)
  } else if (outcome_algorithm == "lm") {
    fit_g <- stats::lm(outcome_formula,
                       data = data_learn,
                       weights = gapclosing.weight)
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.yhat1 = stats::predict(fit_g, newdata = data_estimate_1),
                    gapclosing.yhat0 = stats::predict(fit_g, newdata = data_estimate_0),
                    gapclosing.yhat = dplyr::case_when(gapclosing.treatment == 1 ~ gapclosing.yhat1,
                                            gapclosing.treatment == 0 ~ gapclosing.yhat0),
                    gapclosing.residual = gapclosing.outcome - gapclosing.yhat)
  } else if (outcome_algorithm == "ridge") {
    fit_ridge.out <- fit_ridge(data = data_learn,
                               model_formula = outcome_formula,
                               to_predict = rbind(data_estimate_1,data_estimate_0))
    fit_g <- fit_ridge.out
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.yhat1 = fit_ridge.out$fitted[1:nrow(data_estimate_1)],
                    gapclosing.yhat0 = fit_ridge.out$fitted[(nrow(data_estimate_1) + 1):(nrow(data_estimate_1) + nrow(data_estimate_0))],
                    gapclosing.yhat = dplyr::case_when(gapclosing.treatment == 1 ~ gapclosing.yhat1,
                                                       gapclosing.treatment == 0 ~ gapclosing.yhat0),
                    gapclosing.residual = gapclosing.outcome - gapclosing.yhat)
  } else if (outcome_algorithm == "gam") {
    fit_g <- mgcv::gam(outcome_formula,
                       data = data_learn,
                       weights = gapclosing.weight)
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.yhat1 = stats::predict(fit_g, newdata = data_estimate_1),
                    gapclosing.yhat0 = stats::predict(fit_g, newdata = data_estimate_0),
                    gapclosing.yhat = dplyr::case_when(gapclosing.treatment == 1 ~ gapclosing.yhat1,
                                            gapclosing.treatment == 0 ~ gapclosing.yhat0),
                    gapclosing.residual = gapclosing.outcome - gapclosing.yhat)
  } else if (outcome_algorithm == "ranger") {
    data_learn_0 <- data_learn[data_learn[[treatment_name]] == 0,]
    data_learn_1 <- data_learn[data_learn[[treatment_name]] == 1,]
    if (!all(colnames(data_learn_0) == colnames(data_estimate)) |
        !all(colnames(data_learn_1) == colnames(data_estimate))) {
      stop("Error: data_learn_0, data_learn_1, and data_estimate should have the same columns")
    }
    fit_g_1 <- ranger::ranger(outcome_formula,
                              data = data_learn_1,
                              case.weights = data_learn_1$gapclosing.weight)
    fit_g_0 <- ranger::ranger(outcome_formula,
                              data = data_learn_0,
                              case.weights = data_learn_0$gapclosing.weight)
    fit_g <- list(fit_g_1 = fit_g_1, fit_g_0 = fit_g_0)
    data_estimate <- data_estimate %>%
      dplyr::mutate(gapclosing.yhat1 = stats::predict(fit_g_1, data = data_estimate)$predictions,
                    gapclosing.yhat0 = stats::predict(fit_g_0, data = data_estimate)$predictions,
                    gapclosing.yhat = dplyr::case_when(gapclosing.treatment == 1 ~ gapclosing.yhat1,
                                            gapclosing.treatment == 0 ~ gapclosing.yhat0),
                    gapclosing.residual = gapclosing.outcome - gapclosing.yhat)
  }

  ##########################
  # Aggregate to estimates #
  ##########################

  counterfactual_means <- data_estimate %>%
    # Create the propensity score
    dplyr::mutate(gapclosing.m_i = ifelse(gapclosing.treatment == 1,
                               as.numeric(gapclosing.m_fitted),
                               1 - as.numeric(gapclosing.m_fitted))) %>%
    # Create the counterfactual probability of the factual treatment
    dplyr::mutate(gapclosing.pi_i = dplyr::case_when(gapclosing.treatment == 1 ~ gapclosing.counterfactual_assignments,
                                          gapclosing.treatment == 0 ~ 1 - gapclosing.counterfactual_assignments)) %>%
    # Create the weight that combines all of those
    dplyr::mutate(gapclosing.w_i = gapclosing.weight * gapclosing.pi_i / gapclosing.m_i) %>%
    # Group by category
    dplyr::group_by(dplyr::across(tidyselect::matches(paste0("^",category_name,"$")))) %>%
    # Take weighted means to produce estimates
    dplyr::summarize(outcome_modeling = stats::weighted.mean(gapclosing.yhat1 * gapclosing.counterfactual_assignments +
                                                               gapclosing.yhat0 * (1 - gapclosing.counterfactual_assignments),
                                                             w = gapclosing.weight),
                     treatment_modeling = stats::weighted.mean(gapclosing.outcome,
                                                               w = gapclosing.w_i),
                     gapclosing.robust_augmentation = stats::weighted.mean(-gapclosing.residual,
                                                                w = gapclosing.w_i),
                     .groups = "drop") %>%
    dplyr::mutate(doubly_robust = outcome_modeling - gapclosing.robust_augmentation) %>%
    dplyr::select(-gapclosing.robust_augmentation) %>%
    tidyr::pivot_longer(c("outcome_modeling","treatment_modeling","doubly_robust"),
                        names_to = "method",
                        values_to = "estimate")

  # Difference those to produce gap-closing estimates
  counterfactual_disparities <- foreach::foreach(method_case = unique(counterfactual_means$method), .combine = "rbind") %do% {
    pairwise_diff(category_means_data = counterfactual_means %>%
                    dplyr::filter(method == method_case) %>%
                    dplyr::select(-method),
                  category_name = category_name) %>%
      dplyr::mutate(method = method_case)
  }

  object_to_return <- list(counterfactual_means = counterfactual_means,
                           counterfactual_disparities = counterfactual_disparities,
                           treatment_model = fit_m,
                           outcome_model = fit_g)

  return(object_to_return)
}
