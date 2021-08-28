
# gapclosing

An R package to estimate gap closing estimands. Install this package with the command `devtools::install_github("ilundberg/gapclosing")`.

To get started, see the [vignette](https://ilundberg.github.io/gapclosing/doc/gapclosing.html). Also see the [working paper](https://doi.org/10.31235/osf.io/gx4y3) for which this package is the software implementation.

>Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Working paper available at [https://doi.org/10.31235/osf.io/gx4y3](https://doi.org/10.31235/osf.io/gx4y3).

The goal of this package is to estimate gap-closing estimands. Suppose we want to understand the gap in outcomes between people of different social categories (e.g. racial, gender, or class categories). We want to know the degree to which the gap would close if we intervened to expose them all to some manipulable treatment (e.g. send people to college). To use the gapclosing package, the user defines an intervention, makes assumptions for causal identification, and then specifies models for treatment assignment and/or outcome values as a function of observed confounders. The package estimates the model and aggregates the result to an estimate of the gap-closing estimand. Supported prediction functions include generalized linear models, generalized additive models, ridge regression, and random forest. The package supports doubly-robust estimation and estimation via cross-fitting, to efficiently combine information from treatment and outcome models.