#' Coerce to a Data Frame
#' @description This function converts a \code{gapclosing} object into a data frame. The gapclosing class contains results within a named list, thus simplifying things for manual user interaction with the results. In some programming settings (e.g. a bootstrap), it is easier to work with a rectangular data frame of results. This function produces that data frame.
#' @param x Object of class \code{gapclosing}, produced by a call to \code{gapclosing()}.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A data frame containing estimates.
#' @references Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
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
#'   counterfactual_assignments = 1
#' )
#' summary(estimate)
#'
#' # Convert to a data frame
#' estimate.df <- as.data.frame(estimate)

as.data.frame.gapclosing <- function(x, ...) {

  # Initialize some objects for non-standard evaluation
  estimator <- stringsAsFactors <- change_type <- change_formula <- NULL

  primary_estimator_name <- x$primary_estimator_name

  factual_results_df <- x$factual_means %>%
    dplyr::mutate(estimand = "factual_means") %>%
    # Coerce category to character to match the disparities format
    dplyr::mutate(dplyr::across(x$arguments$category_name, function(value) as.character(value))) %>%
    dplyr::bind_rows(x$factual_disparities %>%
                       dplyr::mutate(estimand = "factual_disparities")) %>%
    dplyr::mutate(estimator = "mean")
  counterfactual_results_df <- do.call(rbind, lapply(names(x$all_estimators), function(estimator_name) {
    this_estimator_results_df <- x$all_estimators[[estimator_name]]$counterfactual_means %>%
      dplyr::mutate(estimand = "counterfactual_means") %>%
      # Coerce category to character to match the disparities format
      dplyr::mutate(dplyr::across(x$arguments$category_name, function(value) as.character(value))) %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$counterfactual_disparities %>%
                         dplyr::mutate(estimand = "counterfactual_disparities")) %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$change_means %>%
                         dplyr::mutate(estimand = paste("change_means",change_type,change_formula, sep = "__")) %>%
                         dplyr::select(-change_type,-change_formula) %>%
                         # Coerce category to character to match the disparities format
                         dplyr::mutate(dplyr::across(x$arguments$category_name, function(value) as.character(value)))) %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$change_disparities %>%
                         dplyr::mutate(estimand = paste("change_disparities",change_type,change_formula, sep = "__")) %>%
                         dplyr::select(-change_type,-change_formula)) %>%
      dplyr::mutate(estimator = estimator_name)
  }))
  data.frame(factual_results_df %>%
               dplyr::bind_rows(counterfactual_results_df) %>%
               dplyr::mutate(primary = estimator %in% c("mean",primary_estimator_name)),
             ...)
}
