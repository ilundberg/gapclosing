#' Gap closing estimator
#' @description A function to estimate gap-closing estimands: means and disparities across categories of units that would persist under some counterfactual assignment of a treatment. To use this function, the user provides a data frame \code{data}, a rule \code{counterfactual_assignments} for counterfactually assigning treatment, a treatment and/or an outcome model for learning statistically about the counterfactuals, and the name \code{category_name} of the variable in \code{data} over which categories are defined. The returned object summarizes factual and counterfactual means and disparities. Supported estimation algorithms include generalized linear models, ridge regression, generalized additive models, and random forests. Standard errors are supported by bootstrapping.
#' @param data Data frame containing the observed data
#' @param counterfactual_assignments Numeric scalar or vector of length nrow(data), each element of which is on the \[0,1\] interval. If a scalar, the counterfactual probability by which all units are assigned to treatment condition 1. If a vector, each element i corresponds to the counterfactual probability by which each unit i is assigned to treatment condition 1.
#' @param outcome_formula Model formula the outcome. Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph). If \code{outcome_algorithm} = "ranger", then the outcome model will be fit separately on the treatment and control groups. Otherwise, the user must specify all interactions in the formula.
#' @param treatment_formula Treatment formula, in the style formula(treatment ~ covariates). Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph).
#' @param category_name Character name of the variable indicating the categories over which the gap is defined. Must be the name of a column in \code{data}.
#' @param outcome_name Character name of the outcome variable. Only required when there is no outcome_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_name Character name of the treatment variable. Only required when there is no treatment_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.
#' @param treatment_algorithm Character name of the algorithm for the treatment model. One of "glm", "ridge", "gam", or "ranger". Defaults to "glm", which is a logit model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}). If "ranger", this function avoids propensity scores equal to 0 or 1 by bottom- and top-coding predicted values at .001 and .999.
#' @param outcome_algorithm Character name of the algorithm for the outcome model. One of "lm", "ridge", "gam", or "ranger". Defaults to "lm", which is an OLS model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}).
#' @param sample_split Character for the type of sample splitting to be conducted. One of "single_sample" or "cross_fit". Defaults to "single_sample", in which case \code{data} is used for both learning the nuisance functions and aggregating to an estimate. Option "cross_fit" uses cross-fitting to repeatedly use part of the sample to learn the nuisance function and another part to estimate the estimand, averaged over repetitions with these roles swapped.
#' @param se Logical indicating whether standard errors should be calculated. Default is FALSE. Standard errors assume a simple random sample within strata of category x treatment, because stratifying by those guarantees that each stratum appears in every bootstrap sample. Standard errors are not supported for weighted samples. Users should consider the data generating process in their specific setting to choosen appropriate method for standard errors.
#' @param bootstrap_samples Only used if \code{se = TRUE}. Number of bootstrap samples. Default is 1000.
#' @param weight_name Character name of a sampling weight variable, if any, which captures the inverse probability of inclusion in the sample. The default assumes a simple random sample (all weights equal).
#' @param n_folds Only used if \code{method} = "cross_fit" and if \code{folds} is not provided. Integer scalar containing number of cross-validation folds. The function will assign observations to folds systematically: sort the data by the variable named \code{category_name}, then by the treatment variable, then at random. On this sorted dataset, folds are assigned systematically by repeated \code{1:n_folds}. To be used if the user does not provide \code{folds}. Defaults to 2.
#' @param folds Only used if \code{method} = "cross_fit". Integer vector containing the fold assignments of each observation. This may be preferable to \code{n_folds} if the researcher has a reason to assign the folds in these data by some other process, perhaps due to particulars of how these data were generated. If null (the default), folds are assigned as stated in \code{n_folds}.
#' @return An object of S3 class \code{gapclosing}, which supports \code{summary()}, \code{print()}, and \code{plot()} functions. The returned object has 7 elements.
#'
#' The first element of the returned list is a data frame \code{primary_estimate} containing the estimates by the primary method. Its columns include:
#' \itemize{
#' \item \code{description} Describes the estimand for each estimate \cr
#' \item \code{setting} An abbreviated version of \code{description} \cr
#' \item \code{category} The category or contrast across categories to which the estimate applies \cr
#' \item \code{estimate} The point estimate \cr
#' \item \code{se} The bootstrapped standard error (if argument \code{se = TRUE}) \cr
#' \item \code{ci.min} lower bound of a 95% confidence interval using \code{se} \cr
#' \item \code{ci.max} upper bound of a 95% confidence interval using \code{se}
#' }
#'
#' After \code{primary_estimate}, the remaining elements of the returned object include:
#' \itemize{
#' \item \code{primary_method} Character denoting the primary method (treatment_modeling, outcome_modeling, or doubly_robust),
#' \item \code{all_estimates} List of estimates by every method used (treatment_modeling, outcome_modeling, doubly_robust)
#' \item \code{treatment_model} If \code{sample_split} = "single_sample", a fitted model object. If \code{sample_split} = "cross_fit", a list of fold-specific fitted model objects.
#' \item \code{outcome_model} If \code{sample_split} = "single_sample", a fitted model object. If \code{sample_split} = "cross_fit", a list of fold-specific fitted model objects.
#' \item \code{estimation_weights} If \code{sample_split} = "single_sample", a numeric vector of length \code{nrow(data)}. Within categories, the weighted average of the outcome with these weights is the treatment modeling estimate of the post-intervention mean defined by \code{counterfactual_assignments}. If \code{sample_split} = "cross_fit", a list of length \code{n_folds} containing the fold-specific treatment modeling weights.
#' \item \code{call} All arguments from the call to \code{gapclosing}.
#' }
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}s
#' @references Friedman J, Hastie T, Tibshirani R (2010). "Regularization Paths for Generalized Linear Models via Coordinate Descent." Journal of Statistical Software, 33(1), 1–22. https://www.jstatsoft.org/v33/i01/.
#' @references Wood S (2017). Generalized Additive Models: An Introduction with R, 2 edition. Chapman and Hall/CRC.
#' @references Wright MN, Ziegler A (2017). "ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R." Journal of Statistical Software, 77(1), 1–17. doi: 10.18637/jss.v077.i01.
#' @export
#'
#' @examples
#' # Simulate example data
#' simulated_data <- generate_simulated_data(n = 100)
#'
#' # Fit by outcome modeling
#' estimate <- gapclosing(
#'   data = simulated_data,
#'   outcome_formula = formula(outcome ~ confounder + category * treatment),
#'   treatment_name = "treatment",
#'   category_name = "category",
#'   counterfactual_assignments = 1,
#'   se = TRUE
#' )
#' summary(estimate)
#'
#' # Fit by treatment modeling
#' # You can add standard errors with se = T
#' estimate <- gapclosing(
#'   data = simulated_data,
#'   treatment_formula = formula(treatment ~ category + confounder),
#'   outcome_name = "outcome",
#'   category_name = "category",
#'   counterfactual_assignments = 1
#' )
#' summary(estimate)
#'
#' # Fit by doubly-robust estimation
#' # You can add standard errors with se = T
#' estimate <- gapclosing(
#'   data = simulated_data,
#'   outcome_formula = formula(outcome ~ confounder + category * treatment),
#'   treatment_formula = formula(treatment ~ category + confounder),
#'   category_name = "category",
#'   counterfactual_assignments = 1
#' )
#' summary(estimate)
#'
#' # Fit by doubly-robust cross-fitting estimation with random forests
#' # You can add standard errors with se = T
#' estimate <- gapclosing(
#'   data = simulated_data,
#'   outcome_formula = formula(outcome ~ confounder + category),
#'   treatment_formula = formula(treatment ~ category + confounder),
#'   category_name = "category",
#'   counterfactual_assignments = 1,
#'   outcome_algorithm = "ranger",
#'   treatment_algorithm = "ranger",
#'   sample_split = "cross_fit"
#' )
#' summary(estimate)

gapclosing <- function(
  data,
  counterfactual_assignments,
  outcome_formula = NULL,
  treatment_formula = NULL,
  category_name,
  outcome_name = NULL,
  treatment_name = NULL,
  treatment_algorithm = "glm",
  outcome_algorithm = "lm",
  sample_split = "single_sample",
  se = FALSE,
  bootstrap_samples = 1000,
  weight_name = NA,
  n_folds = 2,
  folds = NULL
) {
  # Check validity of inputs
  if (!(length(counterfactual_assignments) %in% c(1,nrow(data)))) {
    stop("ERROR: counterfactual_assignments must be length 1 implying the same assignment for everyone or nrow(data) giving the counterfactual assignment probability for each unit.")
  }
  if (!all(counterfactual_assignments >= 0 & counterfactual_assignments <= 1)) {
    stop("ERROR: counterfactual_assignments must be numeric values for probabilities in the range from 0 to 1")
  }
  if (is.null(treatment_formula) & is.null(outcome_formula)) {
    stop("ERROR: At least one of treatment_formula and outcome_formula must be non-null")
  }
  if (is.null(treatment_formula) & is.null(treatment_name)) {
    stop("ERROR: If no treatment_formula is given, you must specify treatment_name")
  }
  if (is.null(outcome_formula) & is.null(outcome_name)) {
    stop("ERROR: If no outcome_formula is given, you must specify outcome_name")
  }
  if (!is.character(category_name)) {
    stop("ERROR: category_name must be a character string")
  }
  if (!(category_name %in% colnames(data))) {
    stop("ERROR: category_name needs to be the name of a column in data")
  }
  if (!(treatment_algorithm %in% c("glm","ridge","gam","ranger"))) {
    stop("ERROR: treatment_algorithm must be one of glm, ridge, gam, ranger")
  }
  if (!(outcome_algorithm %in% c("lm","ridge","gam","ranger"))) {
    stop("ERROR: outcome_algorithm must be one of lm, ridge, gam, ranger")
  }
  if (!(sample_split %in% c("single_sample", "cross_fit"))) {
    stop("ERROR: sample_split must be one of single_sample, cross_fit")
  }
  if (!is.logical(se)) {
    stop("ERROR: se must be TRUE or FALSE")
  }
  if (!is.numeric(bootstrap_samples)) {
    stop("ERROR: bootstrap_samples should be numeric")
  }
  if (se & bootstrap_samples < 1000) {
    warning("You have asked for bootstrap standard errors (se = T) but you have set bootstrap_samples to fewer than 1,000. Although the necessary number of bootstrap samples will differ across applications, you may need to consider increasing bootstrap_samples.")
  }
  if (!is.na(weight_name)) {
    if (!is.character(weight_name)) {
      stop("ERROR: weight_name should be a character string")
    } else if (!(weight_name %in% colnames(data))) {
      stop("ERROR: weight_name should be a column in data")
    }
  }
  if (sample_split == "cross_fit" & is.na(n_folds) & is.null(folds)) {
    stop("ERROR: For cross fitting, one of n_folds or folds is required")
  } else if (sample_split == "cross_fit" & is.null(folds) & !is.numeric(n_folds)) {
    stop("ERROR: n_folds should be numeric")
  }
  if (!is.null(folds)) {
    if (length(folds) != nrow(data)) {
      stop("ERROR: folds should be a vector with nrow(data) elements")
    }
    if (!all(sort(unique(folds)) == 1:max(folds))) {
      stop("ERROR: folds should include all integer values from 1 to max(folds)")
    }
  }


  # Get treatment and outcome names if they are not given
  if (is.null(treatment_name)) {
    treatment_name <- as.character(treatment_formula)[2]
    if (!(treatment_name %in% colnames(data))) {
      stop("ERROR: treatment_name was extracted from treatment_formula, but it appears to not be a column in data")
    }
  } else if (!(treatment_name %in% colnames(data))) {
    stop("ERROR: treatment_name was provided by the user, but it appears to not be a column in data")
  }
  if (is.null(outcome_name)) {
    outcome_name <- as.character(outcome_formula)[2]
    if (!(outcome_name %in% colnames(data))) {
      stop("ERROR: outcome_name was extracted from outcome_formula, but it appears to not be a column in data")
    }
  } else if (!(outcome_name %in% colnames(data))) {
    stop("ERROR: outcome_name was provided by the user, but it appears to not be a column in data")
  }
  # If the treatment is logical, make it numeric
  if (is.logical(data[[treatment_name]])) {
    data[[treatment_name]] <- as.numeric(data[[treatment_name]])
  }

  # Make the point estimate
  if (sample_split == "single_sample") {
    counterfactual_estimate <- point_estimator(
      data_learn = data,
      data_estimate = data,
      outcome_formula = outcome_formula,
      treatment_formula = treatment_formula,
      outcome_name = outcome_name,
      treatment_name = treatment_name,
      category_name = category_name,
      counterfactual_assignments = counterfactual_assignments,
      weight_name = weight_name,
      treatment_algorithm = treatment_algorithm,
      outcome_algorithm = outcome_algorithm
    )
  }
  if (sample_split == "cross_fit") {
    counterfactual_estimate <- cross_fit_estimator(
      data = data,
      counterfactual_assignments = counterfactual_assignments,
      outcome_formula = outcome_formula,
      treatment_formula = treatment_formula,
      category_name = category_name,
      outcome_name = outcome_name,
      treatment_name = treatment_name,
      treatment_algorithm = treatment_algorithm,
      outcome_algorithm = outcome_algorithm,
      weight_name = weight_name,
      n_folds = n_folds,
      folds = folds
    )
  }

  # Make it easier to access the estimated counterfactual means
  counterfactual_means <- counterfactual_estimate$counterfactual_means

  # Calculate the factual estimate
  category_values <- sort(as.character(unique(data[[category_name]])), decreasing = F)
  factual_means_vector <- rep(NA, length(category_values))
  names(factual_means_vector) <- category_values
  for (category_value in category_values) {
    if (is.na(weight_name)) {
      factual_means_vector[category_value] <- mean(data[[outcome_name]][data[[category_name]] == category_value])
    } else {
      factual_means_vector[category_value] <- stats::weighted.mean(data[[outcome_name]][data[[category_name]] == category_value],
                                                                   w = data[[weight_name]][data[[category_name]] == category_value])
    }
  }
  # To facilitate later steps, store this factual estimate in the same matrix form as the counterfactual estimate
  # Note that the columns are identical because the factual does not involve treatment, outcome, or doubly-robust modeling
  factual_means <- matrix(NA,
                          ncol = ncol(counterfactual_means),
                          nrow = nrow(counterfactual_means))
  colnames(factual_means) <- colnames(counterfactual_means)
  rownames(factual_means) <- names(factual_means_vector)
  for (i in 1:ncol(factual_means)) {
    factual_means[,i] <- factual_means_vector
  }

  # Calculate factual and counterfactual disparities
  category_combinations <- utils::combn(sort(category_values, decreasing = T),2)
  counterfactual_disparities <- factual_disparities <-
    matrix(NA, nrow = ncol(category_combinations), ncol = ncol(counterfactual_means))
  colnames(counterfactual_disparities) <- colnames(factual_disparities) <-
    colnames(counterfactual_means)
  rownames(counterfactual_disparities) <- rownames(factual_disparities) <-
    apply(category_combinations, 2, function(x) paste(x, collapse = " - "))
  for (i in 1:ncol(category_combinations)) {
    factual_disparities[i,] <- factual_means[category_combinations[1,i],] -
      factual_means[category_combinations[2,i],]
    counterfactual_disparities[i,] <- counterfactual_estimate$counterfactual_means[category_combinations[1,i],] -
      counterfactual_estimate$counterfactual_means[category_combinations[2,i],]
  }

  # Record the s (factual - counterfactual) and multiplicative changes ((factual - counterfactual) / factual)
  additive_change_means <- factual_means - counterfactual_means
  additive_change_disparities <- factual_disparities - counterfactual_disparities
  multiplicative_change_means <- (factual_means - counterfactual_means) / factual_means
  multiplicative_change_disparities <- (factual_disparities - counterfactual_disparities) / factual_disparities

  # Combine things into a summary data frame for each estimation method
  all_estimates <- list(treatment_modeling = NA, outcome_modeling = NA, doubly_robust = NA)
  for (method in c("treatment_modeling","outcome_modeling","doubly_robust")) {
    all_estimates[[method]] <- rbind(
      data.frame(description = "Factual mean",
                 setting = "factual",
                 category = rownames(factual_means),
                 estimate = factual_means[,method]),
      data.frame(description = "Factual gap",
                 setting = "factual",
                 category = rownames(factual_disparities),
                 estimate = factual_disparities[,method]),
      data.frame(description = "Counterfactual mean",
                 setting = "counterfactual",
                 category = rownames(counterfactual_means),
                 estimate = counterfactual_means[,method]),
      data.frame(description = "Counterfactual gap",
                 setting = "counterfactual",
                 category = rownames(counterfactual_disparities),
                 estimate = counterfactual_disparities[,method]),
      data.frame(description = "Change: Factual - Counterfactual",
                 setting = "change",
                 category = rownames(additive_change_means),
                 estimate = additive_change_means[,method]),
      data.frame(description = "Proportional change: (Factual - Counterfactual) / Factual",
                 setting = "prop_change",
                 category = rownames(multiplicative_change_means),
                 estimate = multiplicative_change_means[,method]),
      data.frame(description = "Amount of gap closed: Factual - Counterfactual",
                 setting = "change",
                 category = rownames(additive_change_disparities),
                 estimate = additive_change_disparities[,method]),
      data.frame(description = "Proportion of gap closed: (Factual - Counterfactual) / Factual",
                 setting = "prop_change",
                 category = rownames(multiplicative_change_disparities),
                 estimate = multiplicative_change_disparities[,method]))
    # Make holder columns for the standard error and confidence interval.
    # If se = T, then these will be filled in later. Otherwise, they will remain NA.
    all_estimates[[method]]$se <- NA
    all_estimates[[method]]$ci.min <- NA
    all_estimates[[method]]$ci.max <- NA
    rownames(all_estimates[[method]]) <- NULL
  }

  # Store the primary estimate
  primary_method <- ifelse(is.null(treatment_formula),
                           "outcome_modeling",
                           ifelse(is.null(outcome_formula),
                                  "treatment_modeling",
                                  "doubly_robust"))
  to_return <- list(
    primary_estimate = all_estimates[[primary_method]],
    primary_method = primary_method,
    all_estimates = all_estimates,
    treatment_model = counterfactual_estimate$treatment_model,
    outcome_model = counterfactual_estimate$outcome_model,
    estimation_weights = counterfactual_estimate$estimation_weights,
    call = list(data = data,
                counterfactual_assignments = counterfactual_assignments,
                outcome_formula = outcome_formula,
                treatment_formula = treatment_formula,
                category_name = category_name,
                outcome_name = outcome_name,
                treatment_name = treatment_name,
                treatment_algorithm = treatment_algorithm,
                outcome_algorithm = outcome_algorithm,
                sample_split = sample_split,
                se = se,
                bootstrap_samples = bootstrap_samples,
                weight_name = weight_name,
                n_folds = n_folds,
                folds = folds)
  )

  # If standard errors are required, bootstrap to add them
  if (se) {
    # Produce standard errors by bootstrapping
    # Generate a matrix to hold bootstrap estimates
    bs_out <- list(treatment_modeling = matrix(NA, nrow = nrow(all_estimates[["treatment_modeling"]]), ncol = bootstrap_samples),
                   outcome_modeling = matrix(NA, nrow = nrow(all_estimates[["outcome_modeling"]]), ncol = bootstrap_samples),
                   doubly_robust = matrix(NA, nrow = nrow(all_estimates[["doubly_robust"]]), ncol = bootstrap_samples))
    # Conduct the bootstrap
    # Draw a bootstrap sample within categories x treatment conditions.
    # The reason to do this is so that no category x treatment condition is empty in a bootstrap sample.
    # It corresponds to sampling variance if the sample was a simple random simple blocked by categories x treatment.
    categories <- unique(data[[category_name]])
    treatment_values <- unique(data[[treatment_name]])
    strata <- data.frame(category = rep(categories, each = length(treatment_values)),
                         treatment = rep(treatment_values, length(categories)))
    strata_indices <- as.list(rep(NA, nrow(strata)))
    for (i in 1:nrow(strata)) {
      strata_indices[[i]] <- which(data[[category_name]] == strata$category[i] & data[[treatment_name]] == strata$treatment[i])
    }
    for (i in 1:bootstrap_samples) {
      # Draw a bootstrap sample within those strata
      chosen <- unlist(lapply(strata_indices, sample, replace = T))
      data_star <- data[chosen,]
      # Apply this function to make the point estimate in the bootstrap sample
      estimate_star <- gapclosing(data = data_star,
                                  outcome_formula = outcome_formula,
                                  treatment_name = treatment_name,
                                  treatment_formula = treatment_formula,
                                  outcome_name = outcome_name,
                                  category_name = category_name,
                                  counterfactual_assignments = counterfactual_assignments,
                                  weight_name = weight_name,
                                  se = F,
                                  treatment_algorithm = treatment_algorithm,
                                  outcome_algorithm = outcome_algorithm,
                                  sample_split = sample_split,
                                  n_folds = n_folds,
                                  folds = folds)
      # Prepare the estimates in the order of the point estimate
      for (method in names(bs_out)) {
        bs_out[[method]][,i] <- estimate_star$all_estimates[[method]]$estimate[which(
          # These lines just ensure that the rows of the bootstrap result are ordered like the rows of the point estimate
          estimate_star$all_estimates[[method]]$setting == to_return$all_estimates[[method]]$setting &
            estimate_star$all_estimates[[method]]$category == to_return$all_estimates[[method]]$category
        )]
      }
    }
    for (method in names(bs_out)) {
      to_return$all_estimates[[method]]$se <- apply(bs_out[[method]],1,stats::sd)
      to_return$all_estimates[[method]]$ci.min <- to_return$all_estimates[[method]]$estimate -
        stats::qnorm(.975) * to_return$all_estimates[[method]]$se
      to_return$all_estimates[[method]]$ci.max <- to_return$all_estimates[[method]]$estimate +
        stats::qnorm(.975) * to_return$all_estimates[[method]]$se
    }
    to_return$primary_estimate <- to_return$all_estimates[[to_return$primary_method]]
  }
  class(to_return) <- "gapclosing"
  return(to_return)
}



