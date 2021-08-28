#' Summary function for gapclosing objects
#' @description Summarizes the S3 class object returned by the gapclosing function
#' @param object An object of class \code{gapclosing}, which results from a call to the function \code{gapclosing}
#' @param ... Other arguments to \code{summary} commands
#' @return Prints a summary of the estimates.
#' @references Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
#' @export

summary.gapclosing <- function(object, ...) {

  # Prepare for non-standard evaluation
  treatment_name <- NULL

  if (is.null(object$arguments$treatment_name)) {
    object$arguments$treatment_name <- all.vars(object$arguments$treatment_formula)[1]
  }
  if (object$arguments$sample_split == "single_sample") {
    cat(paste("Gap-closing estimates using",object$primary_estimator_name,"estimation on one sample.\n"))
  } else {
    cat(paste("Gap-closing estimates using",object$primary_estimator_name,"estimation with cross-fitting.\n"))
  }
  if (object$primary_estimator_name %in% c("treatment_modeling","doubly_robust")) {
    cat(paste("\nTreatment model was",object$arguments$treatment_algorithm,"estimation with model formula:\n"))
    print(object$call$treatment_formula)
  }
  if (object$primary_estimator_name %in% c("outcome_modeling","doubly_robust")) {
    cat(paste0("\nOutcome model was ",object$arguments$outcome_algorithm," estimation ",
              ifelse(object$arguments$outcome_algorithm == "ranger","(estimated separately for each treatment condition) ",""),
              "with model formula:\n"))
    print(object$call$outcome_formula)
  }
  cat(paste0("\nFactual estimates are means within and disparities across ",object$arguments$category_name,".\n"))
  cat(paste0("Counterfactual estimates are under an intervention to set ",treatment_name,
             ifelse(all(object$arguments$counterfactual_assignments == 1), " to 1.\n",
                    ifelse(all(object$arguments$counterfactual_assignments == 0)," to 0.\n",
                           ifelse(length(object$arguments$counterfactual_assignments) == 1, paste("to",object$arguments$counterfactual_assignments),
                                  " by a rule for counterfactual_assignments that varies across individuals.\n"))),
             ifelse(object$arguments$se,paste("Standard errors are calculated from",object$arguments$bootstrap_samples,"bootstrap samples.\n"),"")))
  print(object)
  cat("\nType plot(name_of_this_object) to visualize results.\n")
}
