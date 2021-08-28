pkgname <- "gapclosing"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('gapclosing')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("as.data.frame.gapclosing")
### * as.data.frame.gapclosing

flush(stderr()); flush(stdout())

### Name: as.data.frame.gapclosing
### Title: Coerce to a Data Frame
### Aliases: as.data.frame.gapclosing

### ** Examples

# Simulate example data
simulated_data <- generate_simulated_data(n = 100)

# Fit by outcome modeling
estimate <- gapclosing(
  data = simulated_data,
  outcome_formula = formula(outcome ~ treatment * category + confounder),
  treatment_name = "treatment",
  category_name = "category",
  counterfactual_assignments = 1
)
summary(estimate)

# Convert to a data frame
estimate.df <- as.data.frame(estimate)



cleanEx()
nameEx("df_to_gapclosing_list")
### * df_to_gapclosing_list

flush(stderr()); flush(stdout())

### Name: df_to_gapclosing_list
### Title: Convert Back to Canonical List Output
### Aliases: df_to_gapclosing_list

### ** Examples

# Simulate example data
simulated_data <- generate_simulated_data(n = 100)

# Fit by outcome modeling
estimate <- gapclosing(
  data = simulated_data,
  outcome_formula = formula(outcome ~ treatment * category + confounder),
  treatment_name = "treatment",
  category_name = "category",
  counterfactual_assignments = 1
)
summary(estimate)

# Convert to a data frame
estimate.df <- as.data.frame(estimate)
# Convert back to a list
estimate.df <- df_to_gapclosing_list(estimate.df)



cleanEx()
nameEx("gapclosing")
### * gapclosing

flush(stderr()); flush(stdout())

### Name: gapclosing
### Title: Gap closing estimator
### Aliases: gapclosing

### ** Examples

# Simulate example data
simulated_data <- generate_simulated_data(n = 100)

# Fit by outcome modeling
# You can add standard errors with se = T
estimate <- gapclosing(
  data = simulated_data,
  outcome_formula = outcome ~ treatment * category + confounder,
  treatment_name = "treatment",
  category_name = "category",
  counterfactual_assignments = 1
)
summary(estimate)

# Fit by treatment modeling
# You can add standard errors with se = T
estimate <- gapclosing(
  data = simulated_data,
  treatment_formula = treatment ~ category + confounder,
  outcome_name = "outcome",
  category_name = "category",
  counterfactual_assignments = 1
)
summary(estimate)

# Fit by doubly-robust estimation
# You can add standard errors with se = T
estimate <- gapclosing(
  data = simulated_data,
  outcome_formula = outcome ~ treatment * category + confounder,
  treatment_formula = treatment ~ category + confounder,
  category_name = "category",
  counterfactual_assignments = 1
)
summary(estimate)

# Fit by doubly-robust cross-fitting estimation with random forests
# You can add standard errors with se = T
estimate <- gapclosing(
  data = simulated_data,
  outcome_formula = outcome ~ category + confounder,
  treatment_formula = treatment ~ category + confounder,
  category_name = "category",
  counterfactual_assignments = 1,
  outcome_algorithm = "ranger",
  treatment_algorithm = "ranger",
  sample_split = "cross_fit"
)
summary(estimate)



cleanEx()
nameEx("pairwise_diff")
### * pairwise_diff

flush(stderr()); flush(stdout())

### Name: pairwise_diff
### Title: Pairwise difference calculator
### Aliases: pairwise_diff

### ** Examples

sim_data <- data.frame(example_category = c("A","B","C"),
                       estimate = c(1,2,3))
pairwise_diff(sim_data, category_name = "example_category")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
