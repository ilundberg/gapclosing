

test_that("Test that results hold with arbitrary names for the variables", {
  sim.data <- data.frame(arbitrary_category = c(1,1,1,1,2,2,2,2),
                         # Treat half of each category
                         arbitrary_treatment = c(0,0,1,1,0,0,1,1),
                         irrelevant_covariate = c(0,1,0,1,0,1,0,1),
                         # Define outcomes to have
                         # a gap of 2 under treatment 0
                         # a gap of 0 under treatment 1
                         my_outcome = c(0,0,1,1,2,2,1,1),
                         # Throw in variables named treatment, category, outcome,
                         # which should be just ignored by the function if all is working.
                         treatment = rep(1,8),
                         category = rep(1,8),
                         outcome = rep(1,8))
  estimate_outcome_0 <- gapclosing(sim.data,
                                   counterfactual_assignments = 0,
                                   outcome_formula = formula(my_outcome ~ arbitrary_category*arbitrary_treatment + irrelevant_covariate),
                                   treatment_formula = formula(arbitrary_treatment ~ arbitrary_category + irrelevant_covariate),
                                   category_name = "arbitrary_category")
  estimate_outcome_1 <- gapclosing(sim.data,
                                   counterfactual_assignments = 1,
                                   outcome_formula = formula(my_outcome ~ arbitrary_category*arbitrary_treatment + irrelevant_covariate),
                                   treatment_formula = formula(arbitrary_treatment ~ arbitrary_category + irrelevant_covariate),
                                   category_name = "arbitrary_category")
  expect_equal(sort(abs(estimate_outcome_0$counterfactual_means$estimate)), c(0,2))
  expect_equal(sort(abs(estimate_outcome_1$counterfactual_means$estimate)), c(1,1))
  expect_equal(abs(estimate_outcome_0$counterfactual_disparities$estimate), c(2,2))
  expect_equal(abs(estimate_outcome_1$counterfactual_disparities$estimate), c(0,0))
})
