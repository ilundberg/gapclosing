#' Coerce to a Data Frame
#' @description This function converts a \code{gapclosing} object into a data frame. The gapclosing class contains results within a named list, thus simplifying things for manual user interaction with the results. In some programming settings (e.g. a bootstrap), it is easier to work with a rectangular data frame of results. This function produces that data frame.
#' @param x Object of class \code{gapclosing}, produced by a call to \code{gapclosing()}.
#' @param ... Additional arguments to be passed to or from methods
#' @param stringsAsFactors logical: should the character vector be converted to a factor?
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
#' Convert to a data frame
#' estimate.df <- as.data.frame(estimate)

as.data.frame.gapclosing <- function(x, ...) {
  factual_results_df <- x$factual_means %>%
    mutate(estimand = "factual_means") %>%
    bind_rows(x$factual_disparities %>%
                mutate(estimand = "factual_disparities")) %>%
    mutate(estimator = "mean")
  counterfactual_results_df <- do.call(rbind, lapply(names(x$all_estimators), function(estimator_name) {
    this_estimator_results_df <- x$all_estimators[[estimator_name]]$counterfactual_means %>%
      mutate(estimand = "counterfactual_means") %>%
      bind_rows(x$all_estimators[[estimator_name]]$counterfactual_disparities %>%
                  mutate(estimand = "counterfactual_disparities")) %>%
      bind_rows(x$all_estimators[[estimator_name]]$change_means %>%
                  mutate(estimand = paste("change_means",change_type,change_formula, sep = "__")) %>%
                  select(-change_type,-change_formula)) %>%
      bind_rows(x$all_estimators[[estimator_name]]$change_disparities %>%
                  mutate(estimand = paste("change_disparities",change_type,change_formula, sep = "__")) %>%
                  select(-change_type,-change_formula)) %>%
      mutate(estimator = estimator_name)
  }))
  data.frame(factual_results_df %>%
               bind_rows(counterfactual_results_df) %>%
               mutate(primary = estimator %in% c("mean",x$primary_estimator_name)),
             ...,
             stringsAsFactors = stringsAsFactors)
}
as.data.frame(estimate)
