% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_fit_estimator.R
\name{cross_fit_estimator}
\alias{cross_fit_estimator}
\title{Cross-fitting gap closing estimator}
\usage{
cross_fit_estimator(
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
)
}
\arguments{
\item{data}{Data frame containing the observed data}

\item{counterfactual_assignments}{Numeric scalar or vector of length nrow(data), each element of which is on the [0,1] interval. If a scalar, the counterfactual probability by which all units are assigned to treatment condition 1. If a vector, each element i corresponds to the counterfactual probability by which each unit i is assigned to treatment condition 1.}

\item{outcome_formula}{Model formula the outcome. Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph). If \code{outcome_algorithm} = "ranger", then the outcome model will be fit separately on the treatment and control groups. Otherwise, the user must specify all interactions in the formula.}

\item{treatment_formula}{Treatment formula, in the style formula(treatment ~ covariates). Covariates should include those needed for causal identification of the treatment effect (e.g. as defended in your Directed Acyclic Graph).}

\item{category_name}{Character name of the variable indicating the categories over which the gap is defined. Must be the name of a column in \code{data}.}

\item{outcome_name}{Character name of the outcome variable. Only required when there is no outcome_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.}

\item{treatment_name}{Character name of the treatment variable. Only required when there is no treatment_formula; otherwise extracted automatically. Must be a name of a column in \code{data}.}

\item{treatment_algorithm}{Character name of the algorithm for the treatment model. One of "glm", "ridge", "gam", or "ranger". Defaults to "glm", which is a logit model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}). If "ranger", this function avoids propensity scores equal to 0 or 1 by bottom- and top-coding predicted values at .001 and .999.}

\item{outcome_algorithm}{Character name of the algorithm for the outcome model. One of "lm", "ridge", "gam", or "ranger". Defaults to "lm", which is an OLS model. Option "ridge" is ridge regression. Option "gam" is a generalized additive model fit (see package \code{mgcv}). Option "ranger" is a random forest (see package \code{ranger}).}

\item{weight_name}{Character name of a sampling weight variable, if any, which captures the inverse probability of inclusion in the sample. The default assumes a simple random sample (all weights equal).}

\item{n_folds}{Only used if \code{method} = "cross_fit" and if \code{folds} is not provided. Integer scalar containing number of cross-validation folds. The function will assign observations to folds systematically: sort the data by the variable named \code{category_name}, then by the treatment variable, then at random. On this sorted dataset, folds are assigned systematically by repeated \code{1:n_folds}. To be used if the user does not provide \code{folds}. Defaults to 2.}

\item{folds_name}{Only used if \code{method} = "cross_fit". Character string indicating a column of \code{data} containing fold identifiers. This may be preferable to \code{n_folds} if the researcher has a reason to assign the folds in these data by some other process, perhaps due to particulars of how these data were generated. If null (the default), folds are assigned as stated in \code{n_folds}.}
}
\value{
A list with four elements.
\itemize{
\code{counterfactual_means} A tibble with a counterfactual mean estimate for each category
\code{counterfactual_means} A tibble with a counterfactual disparity estimate for each pair of categories
\code{treatment_model} Object containing the fitted treatment model
\code{outcome_model} Object containing the fitted outcome model
}
}
\description{
This is an internal function typically called from other functions rather than by the user. It creates cross-validation folds and repeatedly calls split_sample_estimator to conduct cross-fitting.
}
\references{
Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
}
