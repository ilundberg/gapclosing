
# gapclosing

<!-- badges: start -->  [![CRAN status](https://www.r-pkg.org/badges/version/gapclosing)](https://CRAN.R-project.org/package=gapclosing) [![](https://cranlogs.r-pkg.org/badges/gapclosing)](https://cran.r-project.org/package=gapclosing)  <!-- badges: end -->

Estimate Gaps Under an Intervention

# Summary

We often want to understand the gap in outcomes between people of different social categories (e.g. racial, gender, or class categories). Further, one would like to know the degree to which the gap would close if we intervened to expose individuals all to some manipulable treatment (e.g. send people to college). This is the focus of the manuscript which is a companion to this package.

>Lundberg, Ian. Forthcoming. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." _Sociological Methods and Research_. Draft available at [https://doi.org/10.31235/osf.io/gx4y3](https://doi.org/10.31235/osf.io/gx4y3).

This R Package provides functions to estimate gap-closing estimands.

The package will enable the user to:

* Estimate treatment and outcome prediction functions with Generalized Linear Models, Generalized Additive Models, ridge regression, or random forest
* Combine those in doubly-robust estimators of gap-closing estimands
* Produce confidence intevals by the bootstrap
* Visualize the result in plots

# Installation instructions

Install from CRAN with one line: `install.packages("gapclosing")`.

To install the latest development version or install before CRAN availability,

1. First, install the `devtools` package: `if(!require(devtools)) install.packages("devtools")`
2. Then, install the `gapclosing` package with the command `devtools::install_github("ilundberg/gapclosing")`.

# Getting started

To get started, see the [vignette](https://ilundberg.github.io/gapclosing/doc/gapclosing.html). Also see the [working paper](https://doi.org/10.31235/osf.io/gx4y3) for which this package is the software implementation.
