
test_that("list to df to list inverts correctly", {
  simulated_data <- generate_simulated_data(n = 100)
  estimate <- gapclosing(
    data = simulated_data,
    outcome_formula = formula(outcome ~ treatment * category + confounder),
    treatment_name = "treatment",
    category_name = "category",
    counterfactual_assignments = 1,
    se = FALSE
  )
  estimate.df <- as.data.frame(estimate)
  estimate.list <- df_to_gapclosing_list(estimate.df)

  # Check that classes are correct
  expect_s3_class(estimate, "gapclosing")
  expect_s3_class(estimate.df, "data.frame")
  expect_equal(class(estimate.list), "list")

  # Check that components are equal
  # Because names in some cases get re-ordered, write a function to do this.
  check_component <- function(original, reconstructed) {
    check1 <- all(sort(names(original)) == sort(names(reconstructed)))
    expect_equal(check1, TRUE)
    expect_equal(original, reconstructed[names(original)])
  }
  for (i in 1:6) {
    check_component(estimate[[i]], estimate.list[[i]])
  }
  for (i in 1:3) {
    check2 <- all(sort(names(estimate$all_estimators[[i]])) == sort(names(estimate.list$all_estimators[[i]])))
    expect_equal(check2, TRUE)
    for (j in names(estimate$all_estimators[[i]])) {
      check_component(estimate$all_estimators[[i]][[j]], estimate.list$all_estimators[[i]][[j]])
    }
  }
})
