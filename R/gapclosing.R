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
#' @param bootstrap_method Only used if \code{se = TRUE}. A character string stating how to conduct bootstrap samples. If "simple", then samples are drawn with replacement from the full data. If "stratified", then the bootstrap is carried out within subpopulations defined by category and treatment.
#' @param parallel_cores Integer number of cores for parallel processing of the bootstrap. Defaults to sequential processing.
#' @param weight_name Character name of a sampling weight variable, if any, which captures the inverse probability of inclusion in the sample. The default assumes a simple random sample (all weights equal).
#' @param n_folds Only used if \code{method} = "cross_fit" and if \code{folds} is not provided. Integer scalar containing number of cross-validation folds. The function will assign observations to folds systematically: sort the data by the variable named \code{category_name}, then by the treatment variable, then at random. On this sorted dataset, folds are assigned systematically by repeated \code{1:n_folds}. To be used if the user does not provide \code{folds}. Defaults to 2.
#' @param folds_name Only used if \code{method} = "cross_fit". Character string indicating a column of \code{data} containing fold identifiers. This may be preferable to \code{n_folds} if the researcher has a reason to assign the folds in these data by some other process, perhaps due to particulars of how these data were generated. If null (the default), folds are assigned as stated in \code{n_folds}.
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
#' @importFrom foreach %do%
#' @importFrom foreach %dopar%
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # Simulate example data
#' simulated_data <- generate_simulated_data(n = 100)
#'
#' # Fit by outcome modeling
#' estimate <- gapclosing(
#'   data = simulated_data,
#'   outcome_formula = formula(outcome ~ treatment * category + confounder),
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
#'   outcome_formula = formula(outcome ~ treatment * category + confounder),
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
#'   outcome_formula = formula(outcome ~ category + confounder),
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
  bootstrap_samples = 10,#00, # UPDATE THIS IN FINAL VERSION
  bootstrap_method = "simple",
  parallel_cores = NULL,
  weight_name = NULL,
  n_folds = 2,
  folds_name = NULL
) {

  ############################
  # Save arguments to return #
  ############################

  arguments <- list(
    data = data,
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
    bootstrap_method = bootstrap_method,
    parallel_cores = parallel_cores,
    weight_name = weight_name,
    n_folds = n_folds,
    folds_name = folds_name
  )

  # Initialize some objects for non-standard evaluation
  gapclosing.weight <- estimate <- category <- f <- method <- additive <-
    proportional <- change_type <- change_formula <- method <- i <- NULL

  ############################
  # Check validity of inputs #
  ############################

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
  if (!(bootstrap_method %in% c("simple","stratified"))) {
    stop("ERROR: bootstrap_method should be one of simple or stratified")
  }
  if (!is.null(weight_name)) {
    if (!is.character(weight_name)) {
      stop("ERROR: weight_name should be a character string")
    } else if (!(weight_name %in% colnames(data))) {
      stop("ERROR: weight_name should be a column in data")
    }
  }
  if (sample_split == "cross_fit" & is.na(n_folds) & is.null(folds_name)) {
    stop("ERROR: For cross fitting, one of n_folds or folds_name is required")
  } else if (sample_split == "cross_fit" & is.null(folds_name) & !is.numeric(n_folds)) {
    stop("ERROR: n_folds should be numeric")
  }
  if (!is.null(folds_name)) {
    if (!(folds_name %in% names(data))) {
      stop("ERROR: folds_name should be a column of data")
    }
  }

  #################
  # Modify inputs #
  #################

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

  ##########################################
  # Make the counterfactual point estimate #
  ##########################################

  if (sample_split == "single_sample") {
    counterfactual_point <- point_estimator(
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
    counterfactual_point <- cross_fit_estimator(
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
      folds_name = folds_name
    )
  }

  ##############################################
  # Estimate the factual means and disparities #
  ##############################################

  if (is.null(weight_name)) {
    factual_means <- data %>%
    dplyr::group_by(dplyr::across(tidyselect::matches(paste0("^",category_name,"$")))) %>%
      dplyr::summarize(dplyr::across(tidyselect::matches(paste0("^",outcome_name,"$")), mean),
                       .groups = "drop") %>%
      dplyr::rename_with(function(x) dplyr::case_when(x == outcome_name ~ "estimate",
                                                      T ~ x))
  } else {
    factual_means <- data %>%
      dplyr::mutate(gapclosing.weight = data[[weight_name]]) %>%
      dplyr::group_by(dplyr::across(tidyselect::matches(paste0("^",category_name,"$")))) %>%
      dplyr::summarize(dplyr::across(tidyselect::matches(paste0("^",outcome_name,"$")), stats::weighted.mean, w = gapclosing.weight),
                       .groups = "drop") %>%
      dplyr::rename_with(function(x) dplyr::case_when(x == outcome_name ~ "estimate",
                                                      T ~ x))
  }


  factual_disparities <- pairwise_diff(factual_means, category_name = category_name)

  ####################################################
  # Calculate changes from factual to counterfactual #
  ####################################################

  # Write a function that will be applied separately to means and disparities
  calculate_changes <- function(factual_estimates, counterfactual_estimates) {
    counterfactual_estimates %>%
      dplyr::rename(c = estimate) %>%
      dplyr::left_join(factual_estimates %>%
                         dplyr::select(tidyselect::all_of(c(category_name,"estimate"))) %>%
                         dplyr::rename(f = estimate),
                       by = category_name) %>%
      dplyr::mutate(additive = f - c,
                    proportional = (f - c) / f) %>%
      dplyr::select(category,method,additive,proportional) %>%
      tidyr::pivot_longer(cols = c("additive","proportional"),
                          names_to = "change_type",
                          values_to = "estimate") %>%
      dplyr::mutate(change_formula = dplyr::case_when(change_type == "additive" ~ "f-c",
                                                      change_type == "proportional" ~ "(f-c)/f")) %>%
      dplyr::select(category,method,change_type,change_formula,estimate)
  }

  change_means <- calculate_changes(factual_estimates = factual_means,
                                    counterfactual_estimates = counterfactual_point$counterfactual_means)

  change_disparities <- calculate_changes(factual_estimates = factual_disparities,
                                          counterfactual_estimates = counterfactual_point$counterfactual_disparities)

  ##############################
  # Create an object to return #
  ##############################

  # Identify the primary method to return. Others will be returned as alternatives.
  primary_estimator_name <- dplyr::case_when(!is.null(treatment_formula) & !is.null(outcome_formula) ~ "doubly_robust",
                                             !is.null(outcome_formula) ~ "outcome_modeling",
                                             !is.null(treatment_formula) ~ "treatment_modeling")

  # Organize results by estimator instead of by estimand
  all_estimators <- lapply(
    c(outcome_modeling = "outcome_modeling",
      treatment_modeling = "treatment_modeling",
      doubly_robust = "doubly_robust"),
    function(method_case) {
      lapply(
        list(counterfactual_means = counterfactual_point$counterfactual_means,
             counterfactual_disparities = counterfactual_point$counterfactual_disparities,
             change_means = change_means,
             change_disparities = change_disparities),
        function(x) {
          x %>%
            dplyr::filter(method == method_case) %>%
            dplyr::select(-method)
        }
      )
    }
  )

  # Prepare a list to return
  to_return <- c(list(factual_means = factual_means,
                      factual_disparities = factual_disparities),
                 all_estimators[[primary_estimator_name]],
                 list(primary_estimator_name = primary_estimator_name,
                      all_estimators = all_estimators),
                 treatment_model = list(counterfactual_point$treatment_model),
                 outcome_model = list(counterfactual_point$outcome_model),
                 call = match.call(),
                 arguments = list(arguments))

  # If se = F, the next bit is skipped and the function returns to_return.
  # If se = T, the next bit adds the standard errors before returning
  # by repeatedly calling this function with a bootstrap sample and se = F.

  ##################################################################
  # Bootstrap standard errors by recursively calling this function #
  ##################################################################
  if (se) {
    # Prepare to run bootstrap in parallel if requested by the user
    if (!is.null(parallel_cores)) {
      cl <- parallel::makeCluster(parallel_cores)
      doParallel::registerDoParallel(cl)
      `%domethod%` <- `%dopar%`
    } else {
      `%domethod%` <- `%do%`
    }
    # Produce standard errors by bootstrapping
    bs_estimates <- foreach::foreach(i = 1:bootstrap_samples) %domethod% {
      if (i %% bootstrap_samples == 100) {
        print(paste("Beginning draw",i,"of",bootstrap_samples))
      }
      if (bootstrap_method == "simple") {
        data_star <- data %>%
          dplyr::slice_sample(prop = 1, replace = T)
      } else if (bootstrap_method == "stratified") {
        # This is stratified sampling within category x treatment.
        # This ensures that every cell contains data and avoids yielding se = NA
        # if some of the cells do not contain data in some samples.
        data_star <- data %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(c(category_name,treatment_name)))) %>%
          dplyr::slice_sample(prop = 1, replace = T) %>%
          dplyr::ungroup()
      }
      gapclosing_star <- gapclosing(data = data_star,
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
                                    folds_name = folds_name)
      gapclosing_star[c(1:6,8)]
    }
    if (!is.null(parallel_cores)) {
      parallel::stopCluster(cl)
      rm(cl)
    }

    # Combine those into standard errors
    # First, write a function to convert a list of data frames into a single data frame with standard errors
    make_se <- function(list_of_df) {
      # Convert the list of data frames to one long data frame
      long_df <- do.call(rbind, list_of_df)
      # Determine grouping variables: all variables except the estimate
      grouping_variables <- names(long_df %>% dplyr::select(-estimate))
      # Calculate the standard error
      return(long_df %>%
               dplyr::group_by(dplyr::across(tidyselect::all_of(grouping_variables))) %>%
               dplyr::summarize(se = stats::sd(estimate),
                                .groups = "drop"))
    }
    # Get a named list of element names that need standard errors
    element_names <- names(bs_estimates[[1]])
    names(element_names) <- element_names
    # Calculate the standard errors by the standard deviation across bootstrap samples.
    # This is complicated only because of the list structure.
    se_estimate <- lapply(element_names, function(return_element) {
      #      foreach::foreach(return_element = names(bs_estimates[[1]])) %do% {
      if (return_element != "all_estimators") {
        make_se(lapply(bs_estimates, function(x) x[[return_element]]))
      } else if (return_element == "all_estimators") {
        estimator_cases <- c(outcome_modeling = "outcome_modeling",
                             treatment_modeling = "treatment_modeling",
                             doubly_robust = "doubly_robust")
        estimand_cases <- c(counterfactual_means = "counterfactual_means",
                            counterfactual_disparities = "counterfactual_disparities",
                            change_means = "change_means",
                            change_disparities = "change_disparities")
        lapply(estimator_cases, function(estimator_case) {
          lapply(estimand_cases, function(estimand_case) {
            make_se(lapply(bs_estimates, function(x) x$all_estimators[[estimator_case]][[estimand_case]]))
          })
        })
      }
    })
    # Append those standard errors to the point estimates
    for (element_name in c("factual_means","factual_disparities","counterfactual_means","counterfactual_disparities",
                           "change_means","change_disparities")) {
      grouping_variables <- colnames(to_return[[element_name]] %>% dplyr::select(-estimate))
      to_return[[element_name]] <- to_return[[element_name]] %>%
        dplyr::left_join(se_estimate[[element_name]], by = grouping_variables) %>%
        dplyr::mutate(ci.min = estimate - stats::qnorm(.975) * se,
                      ci.max = estimate + stats::qnorm(.975) * se)
    }
    for (estimator_name in c("outcome_modeling","treatment_modeling","doubly_robust")) {
      for (element_name in c("counterfactual_means","counterfactual_disparities","change_means","change_disparities")) {
        grouping_variables <- colnames(to_return[["all_estimators"]][[estimator_name]][[element_name]] %>% dplyr::select(-estimate))
        to_return[["all_estimators"]][[estimator_name]][[element_name]] <- to_return[["all_estimators"]][[estimator_name]][[element_name]] %>%
          dplyr::left_join(se_estimate[["all_estimators"]][[estimator_name]][[element_name]], by = grouping_variables) %>%
          dplyr::mutate(ci.min = estimate - stats::qnorm(.975) * se,
                        ci.max = estimate + stats::qnorm(.975) * se)
      }
    }
  }
  class(to_return) <- "gapclosing"
  return(to_return)
}



