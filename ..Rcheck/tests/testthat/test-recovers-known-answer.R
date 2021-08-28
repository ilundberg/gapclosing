

test_that("Outcome modeling recovers known answer", {
  sim.data <- data.frame(category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         treatment = c(0,0,1,1,0,0,1,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         outcome = c(0,0,1,1,2,2,1,1))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment")
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment")
  expect_equal(sort(abs(estimate_outcome_0$counterfactual_means$estimate)), c(0,2))
  expect_equal(sort(abs(estimate_outcome_1$counterfactual_means$estimate)), c(1,1))
  expect_equal(abs(estimate_outcome_0$counterfactual_disparities$estimate), c(2,2))
  expect_equal(abs(estimate_outcome_1$counterfactual_disparities$estimate), c(0,0))
})

test_that("Treatment modeling recovers known answer", {
  sim.data <- data.frame(category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         treatment = c(0,0,1,1,0,0,1,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         outcome = c(0,0,1,1,2,2,1,1))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   treatment_formula = formula(treatment ~ category),
                                   category_name = "category",
                                   outcome_name = "outcome")
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   treatment_formula = formula(treatment ~ category),
                                   category_name = "category",
                                   outcome_name = "outcome")
  expect_equal(sort(abs(estimate_outcome_0$counterfactual_means$estimate)), c(0,2))
  expect_equal(sort(abs(estimate_outcome_1$counterfactual_means$estimate)), c(1,1))
  expect_equal(abs(estimate_outcome_0$counterfactual_disparities$estimate), c(2,2))
  expect_equal(abs(estimate_outcome_1$counterfactual_disparities$estimate), c(0,0))
})

test_that("Doubly robust recovers known answer", {
  sim.data <- data.frame(category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         treatment = c(0,0,1,1,0,0,1,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         outcome = c(0,0,1,1,2,2,1,1))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   treatment_formula = formula(treatment ~ category),
                                   category_name = "category")
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   treatment_formula = formula(treatment ~ category),
                                   category_name = "category")
  expect_equal(sort(abs(estimate_outcome_0$counterfactual_means$estimate)), c(0,2))
  expect_equal(sort(abs(estimate_outcome_1$counterfactual_means$estimate)), c(1,1))
  expect_equal(abs(estimate_outcome_0$counterfactual_disparities$estimate), c(2,2))
  expect_equal(abs(estimate_outcome_1$counterfactual_disparities$estimate), c(0,0))
})
