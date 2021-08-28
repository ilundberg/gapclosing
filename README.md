
# gapclosing

Estimate Gaps Under an Intervention

# Summary

Provides functions to estimate gap-closing estimands: the disparities across categories (e.g. Black and white) that persists if a treatment variable (e.g. college) is equalized. The purpose is to estimate the average outcomes that units would realize if exposed to a counterfactual treatment assignment rule.

The package will enable the user to:

* Estimate treatment and outcome prediction functions with Generalized Linear Models, Generalized Additive Models, ridge regression, or random forest
* Combine those in doubly-robust estimators of gap-closing estimands
* Produce confidence intevals by the bootstrap
* Visualize the result in plots

# Installation instructions

When the package becomes available on CRAN, you will be able to install with one line: `install.packages("gapclosing")`.

To install the latest development version or install before CRAN availability,

1. First, install the `devtools` package: `if(!require(devtools)) install.packages("devtools")`
2. Then, install the `gapclosing` package with the command `devtools::install_github("ilundberg/gapclosing")`.

# Getting started

To get started, see the [vignette](https://ilundberg.github.io/gapclosing/doc/gapclosing.html). Also see the [working paper](https://doi.org/10.31235/osf.io/gx4y3) for which this package is the software implementation.

>Lundberg, Ian. Forthcoming. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." _Sociological Methods and Research_. Draft available at [https://doi.org/10.31235/osf.io/gx4y3](https://doi.org/10.31235/osf.io/gx4y3).
