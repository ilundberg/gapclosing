#' Summary function for gapclosing objects
#' @description Summarizes the S3 class object returned by the gapclosing function
#' @param object An object of class \code{gapclosing}, which results from a call to the function \code{gapclosing}
#' @param ... Other arguments to \code{summary} commands
#' @return Prints a summary of the estimates.
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @export

summary.gapclosing <- function(object, ...) {
  if (object$call$sample_split == "single_sample") {
    cat(paste("Gap-closing estimates using",object$primary_method,"estimation on one sample.\n"))
  } else {
    cat(paste("Gap-closing estimates using",object$primary_method,"estimation with cross-fitting.\n"))
  }
  if (object$primary_method %in% c("treatment_modeling","doubly_robust")) {
    cat(paste("\nTreatment model was",object$call$treatment_algorithm,"estimation with model formula:\n"))
    print(object$call$treatment_formula)
  }
  if (object$primary_method %in% c("outcome_modeling","doubly_robust")) {
    cat(paste("\nOutcome model was",object$call$outcome_algorithm,"estimation",
              ifelse(object$call$outcome_algorithm == "ranger","(estimated separately for each treatment condition)",""),
              "with model formula:\n"))
    print(object$call$outcome_formula)
  }
  cat(paste0("\nFactual estimates are means within and disparities across ",object$call$category_name,".\n"))
  cat(paste0("Counterfactual estimates are under an intervention to set ",object$call$treatment_name,
             ifelse(all(object$call$counterfactual_assignments == 1), " to 1.\n",
                    ifelse(all(object$call$counterfactual_assignments == 0)," to 0.\n",
                           ifelse(length(object$call$counterfactual_assignments) == 1, paste("to",object$call$counterfactual_assignments),
                                  " by a rule for counterfactual_assignments that varies across individuals.\n"))),
             ifelse(object$call$se,paste("Standard errors are calculated from",object$call$bootstrap_samples,"bootstrap samples.\n"),""),
             "\nThis table is stored in the primary_estimate slot of this object.\nType plot(name_of_this_object) to visualize results.\n\n"))
  print(object)
}
