#' Convert Back to Canonical List Output
#' @description If the user has used \code{as.data.frame(x)} to convert a \code{gapclosing} object to a data frame of estimates, this function will invert back to the original list format. This function does not fully reinstate the original gapclosing object because some elements are lost when \code{as.data.frame()} is called. This function is most useful as a check on \code{as.data.frame()} and as a helper in settings like bootstrapping where a data frame is easier to work with but we want to return to the original format before returning an object to the user.
#' @param x A data frame produced by \code{as.data.frame(x)} applied to an object \code{x} of class \code{gapclosing}.
#' @return A list containing a subset of the elements in a \code{gapclosing} object.
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
#' Convert back to a list
#' estimate.df <- df_to_gapclosiing_list(estimate.df)

df_to_gapclosing_list <- function(x) {
  factual <- list(factual_means = x %>%
                    filter(estimand == "factual_means") %>%
                    select(-estimand,-estimator,-primary),
                  factual_disparities = x %>%
                    filter(estimand == "factual_disparities") %>%
                    select(-estimand,-estimator,-primary))
  estimator_names <- unique(x$estimator)
  estimator_names <- estimator_names[estimator_names != "mean"]
  all_estimators <- lapply(estimator_names, function(estimator_case) {
    list(counterfactual_means = x %>%
           filter(estimand == "counterfactual_means" & estimator == estimator_case) %>%
           select(-estimand,-estimator,-primary),
         counterfactual_disparities = x %>%
           filter(estimand == "counterfactual_disparities" & estimator == estimator_case) %>%
           select(-estimand,-estimator,-primary),
         change_means = x %>%
           filter(grepl("change_means",estimand) & estimator == estimator_case) %>%
           separate(estimand, into = c("estimand", "change_type", "change_formula"), sep = "__") %>%
           select(-estimand,-estimator,-primary),
         change_disparities = x %>%
           filter(grepl("change_disparities",estimand) & estimator == estimator_case) %>%
           separate(estimand, into = c("estimand", "change_type", "change_formula"), sep = "__") %>%
           select(-estimand,-estimator,-primary))
  })
  names(all_estimators) <- estimator_names
  primary_estimator_name <- unique(x$estimator[x$primary & x$estimator != "mean"])
  return(c(factual,
           all_estimators[[primary_estimator_name]],
           all_estimators = list(all_estimators)))
}
