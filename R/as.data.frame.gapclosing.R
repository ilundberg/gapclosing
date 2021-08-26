#' Coerce to a Data Frame
#' @description This function converts a \code{gapclosing} object into a data frame. The gapclosing class contains results within a named list, thus simplifying things for manual user interaction with the results. In some programming settings (e.g. a bootstrap), it is easier to work with a rectangular data frame of results. This function produces that data frame.
#' @param x Object of class \code{gapclosing}, produced by a call to \code{gapclosing()}.
#' @param row.names \code{NULL} or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting column names (to syntactic names: see \code{make.names}) is optional. Note that all of R's base package \code{as.data.frame()} methods use \code{optional} only for column names treatment, basically with the meaning of \code{data.frame(*, check.names = !optional)}. See also the \code{make.names} argument of the \code{matrix} method.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A data frame containing estimates.
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
#' # Convert to a data frame
#' estimate.df <- as.data.frame(estimate)

as.data.frame.gapclosing <- function(x, row.names = NULL, optional = FALSE, ...) {

  # Initialize some objects for non-standard evaluation
  estimator <- stringsAsFactors <- change_type <- change_formula <- NULL

  factual_results_df <- x$factual_means %>%
    dplyr::mutate(estimand = "factual_means") %>%
    dplyr::bind_rows(x$factual_disparities %>%
                       dplyr::mutate(estimand = "factual_disparities")) %>%
    dplyr::mutate(estimator = "mean")
  counterfactual_results_df <- do.call(rbind, lapply(names(x$all_estimators), function(estimator_name) {
    this_estimator_results_df <- x$all_estimators[[estimator_name]]$counterfactual_means %>%
      dplyr::mutate(estimand = "counterfactual_means") %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$counterfactual_disparities %>%
                         dplyr::mutate(estimand = "counterfactual_disparities")) %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$change_means %>%
                         dplyr::mutate(estimand = paste("change_means",change_type,change_formula, sep = "__")) %>%
                         dplyr::select(-change_type,-change_formula)) %>%
      dplyr::bind_rows(x$all_estimators[[estimator_name]]$change_disparities %>%
                         dplyr::mutate(estimand = paste("change_disparities",change_type,change_formula, sep = "__")) %>%
                         dplyr::select(-change_type,-change_formula)) %>%
      dplyr::mutate(estimator = estimator_name)
  }))
  data.frame(factual_results_df %>%
               dplyr::bind_rows(counterfactual_results_df) %>%
               dplyr::mutate(primary = estimator %in% c("mean",x$primary_estimator_name)),
             row.names = NULL, optional = FALSE, ...)
}
