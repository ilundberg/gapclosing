#' Cross-fitting gap closing estimator
#' @description This is an internal function typically called from other functions rather than by the user. It creates cross-validation folds and repeatedly calls split_sample_estimator to conduct cross-fitting.
#' @param data Data frame containing the observed data
#' @param counterfactual_assignments Numeric scalar or vector of length nrow(data), each element of which is on the \[0,1\] interval. If a scalar, the counterfactual probability by which all units are assigned to treatment condition 1. If a vector, each element i corresponds to the counterfactual probability by which each unit i is assigned to treatment condition 1.
#' @param outcome_formula Model formula the outcome. Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph). If \code{outcome_algorithm} = "ranger", then the outcome model will be fit separately on the treatment and control groups. Otherwise, the user must specify all interactions in the formula.
#' @param treatment_formula Treatment formula, in the style formula(treatment ~ covariates). Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph).
#' @param category_name Character name of the variable indicating the categories over which the gap is defined. Must be the name of a column in \code{data}.
#' @param outcome_name Character name of the outcome variable. Only required when there is no outcome_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_name Character name of the treatment variable. Only required when there is no treatment_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_algorithm Character name of the algorithm for the treatment model. One of "glm", "ridge", "gam", or "ranger". Defaults to "glm", which is a logit model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}). If "ranger", this function avoids propensity scores equal to 0 or 1 by bottom- and top-coding predicted values at .001 and .999.
#' @param outcome_algorithm Character name of the algorithm for the outcome model. One of "lm", "ridge", "gam", or "ranger". Defaults to "lm", which is an OLS model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}).
#' @param weight_name Character name of a sampling weight variable, if any, which captures the inverse probability of inclusion in the sample. The default assumes a simple random sample (all weights equal).
#' @param n_folds Only used if \code{method} = "cross_fit" and if \code{folds} is not provided. Integer scalar containing number of cross-validation folds. The function will assign observations to folds systematically: sort the data by the variable named \code{category_name}, then by the treatment variable, then at random. On this sorted dataset, folds are assigned systematically by repeated \code{1:n_folds}. To be used if the user does not provide \code{folds}. Defaults to 2.
#' @param folds_name Only used if \code{method} = "cross_fit". Character string indicating a column of \code{data} containing fold identifiers. This may be preferable to \code{n_folds} if the researcher has a reason to assign the folds in these data by some other process, perhaps due to particulars of how these data were generated. If null (the default), folds are assigned as stated in \code{n_folds}.
#' @return A list with four elements.
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

cross_fit_estimator <- function(
  data,
  counterfactual_assignments,
  outcome_formula,
  treatment_formula,
  category_name,
  outcome_name,
  treatment_name,
  treatment_algorithm = "glm",
  outcome_algorithm = "lm",
  weight_name = NULL,
  n_folds = 2,
  folds_name = NULL
) {
  # Initialize values to silence errors from non-standard evaluations
  gapclosing.folds <- f <- estimate <- NULL

  # If explicit fold assignments were not given, assign folds here
  if (!is.null(folds_name)) {
    n_folds <- length(unique(data[[folds_name]]))
    data$gapclosing.folds <- data[[folds_name]]
  }
  if (is.null(folds_name)) {
    # Use a cross-validation design to balance folds across categories and treatments
    # Sort data systematically by category and treatment, then randomly within those
    data_sort <- order(data[[category_name]],data[[treatment_name]],stats::runif(nrow(data)))
    data <- data[data_sort,]
    data$gapclosing.folds <- rep(1:n_folds, ceiling(nrow(data) / n_folds))[1:nrow(data)]
  }
  # Initialize a list to hold the fold estimates
  fold_estimates <- foreach::foreach(f = unique(data$gapclosing.folds)) %do% {
    point_estimator(
      data_learn = data %>% dplyr::filter(gapclosing.folds != f),
      data_estimate = data %>% dplyr::filter(gapclosing.folds == f),
      outcome_formula = outcome_formula,
      treatment_formula = treatment_formula,
      outcome_name = outcome_name,
      treatment_name = treatment_name,
      category_name = category_name,
      counterfactual_assignments = ifelse(length(counterfactual_assignments == 1),
                                          counterfactual_assignments,
                                          counterfactual_assignments[data$gapclosing.folds == f]),
      weight_name = weight_name,
      treatment_algorithm = treatment_algorithm,
      outcome_algorithm = outcome_algorithm
    )
  }
  names(fold_estimates) <- paste0("fold_",unique(data$gapclosing.folds),"_left_out")

  # Pool the counterfactual_means and counterfactual_disparities results across cross-fits
  pooled <- lapply(
    c(counterfactual_means = "counterfactual_means", counterfactual_disparities = "counterfactual_disparities"),
    function(element) {
      do.call(rbind,lapply(fold_estimates, function(fold_result) fold_result[[element]])) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(c(category_name,"method")))) %>%
        dplyr::summarize(estimate = mean(estimate),
                         .groups = "drop")
    }
  )

  object_to_return <- list(counterfactual_means = pooled$counterfactual_means,
                           counterfactual_disparities = pooled$counterfactual_disparities,
                           treatment_model = lapply(fold_estimates, function(x) x$treatment_model),
                           outcome_model = lapply(fold_estimates, function(x) x$outcome_model))

  return(object_to_return)
}


