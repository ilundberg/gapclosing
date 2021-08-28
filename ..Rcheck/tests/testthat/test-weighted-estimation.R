test_that("Test weighted estimation", {
  sim.data <- data.frame(category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         treatment = c(0,0,1,1,0,0,1,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         outcome = c(0,0,1,1,2,2,1,1),
                         # Triple weight on untreated cases
                         my_weight = c(3,3,1,1,3,3,1,1))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment",
                                   weight_name = "my_weight")
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment",
                                   weight_name = "my_weight")
  # Check that factual disparities correctly use weights
  expect_equal(abs(estimate_outcome_0$factual_disparities$estimate), c(1.5,1.5))
  expect_equal(abs(estimate_outcome_1$factual_disparities$estimate), c(1.5,1.5))
  # Check that counterfactual disparity is still 2 in treatment 0 and 0 in treatment 1
  expect_equal(abs(estimate_outcome_0$counterfactual_disparities$estimate), c(2,2))
  expect_equal(abs(estimate_outcome_1$counterfactual_disparities$estimate), c(0,0))

  # Test in a setting where the weights vary within (category x treatment)
  sim.data <- data.frame(category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         treatment = c(0,0,1,1,0,0,1,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         outcome = c(0,4,0,8,0,4,0,4),
                         # Triple weight on untreated cases
                         my_weight = c(1,3,1,3,1,3,1,3))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment",
                                   weight_name = "my_weight")
  expect_equal(sort(estimate_outcome_0$counterfactual_means$estimate), c(3,3))
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   outcome_formula = formula(outcome ~ category*treatment),
                                   category_name = "category",
                                   treatment_name = "treatment",
                                   weight_name = "my_weight")
  expect_equal(sort(estimate_outcome_1$counterfactual_means$estimate), c(3,6))
})
