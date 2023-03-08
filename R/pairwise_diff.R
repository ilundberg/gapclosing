#' Pairwise difference calculator
#' @description A function to estimate the pairwise differences of estimates made for each category.
#' @param category_means_data Data frame containing two columns: the category-specific mean estimate (a column named \code{estimate}) and the category name (named as specified in \code{category_name})
#' @param category_name The name of the column containing the category identifier.
#' @return A data frame with pairwise differences of \code{estimate} over pairs of categories.
#' @references Lundberg I (2021). "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." Sociological Methods and Research. Available at {https://osf.io/gx4y3/}.
#' @references Friedman J, Hastie T, Tibshirani R (2010). "Regularization Paths for Generalized Linear Models via Coordinate Descent." Journal of Statistical Software, 33(1), 1–22. {https://www.jstatsoft.org/htaccess.php?volume=33&type=i&issue=01}.
#' @references Wood S (2017). Generalized Additive Models: An Introduction with R, 2 edition. Chapman and Hall/CRC.
#' @references Wright MN, Ziegler A (2017). "ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R." Journal of Statistical Software, 77(1), 1–17. doi: 10.18637/jss.v077.i01.
#' @importFrom foreach %do%
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' sim_data <- data.frame(example_category = c("A","B","C"),
#'                        estimate = c(1,2,3))
#' pairwise_diff(sim_data, category_name = "example_category")

pairwise_diff <- function(category_means_data, category_name) {

  # Check arguments
  if (!("estimate" %in% colnames(category_means_data))) {
    stop("Error: category_means_data should have a column named estimate containing the mean outcome in that category")
  }
  if (!(category_name %in% colnames(category_means_data))) {
    stop("Error: category_name should be a character string with the name of a column in category_means_data")
  }

  # Prepare for non-standard evaluation
  category1 <- category2 <- category <- category1_value <- category2_value <- estimate <- NULL

  # Prepare a data frame with the category differences
  result <- category_means_data %>%
    # Create a data frame with all category values
    dplyr::select(tidyselect::matches(paste0("^",category_name,"$"))) %>%
    # Call that category1
    dplyr::rename_with(function(x) "category1") %>%
    # Sort that data frame
    dplyr::arrange(category1) %>%
    # Create a second category value identical to the first
    dplyr::mutate(category2 = category1) %>%
    # Expand to have all combinations of the two categories
    tidyr::expand(category1,category2) %>%
    # Remove cases where both categories are the same
    dplyr::filter(category1 != category2) %>%
    # Merge in the factual mean for category 1
    dplyr::left_join(category_means_data %>%
                       dplyr::rename(category1 = category_name,
                                     category1_value = estimate),
                     by = "category1") %>%
    # Merge in the factual mean for category 2
    dplyr::left_join(category_means_data %>%
                       dplyr::rename(category2 = category_name,
                                     category2_value = estimate),
                     by = "category2") %>%
    # Produce a difference estimate and give it a name
    dplyr::mutate(difference_name = paste(category1,"-",category2),
                  estimate = category1_value - category2_value) %>%
    # Rename to make the output align with the input
    dplyr::rename_with(function(x) dplyr::case_when(x == "difference_name" ~ category_name,
                                                    x == "estimate" ~ "estimate",
                                                    T ~ x)) %>%
    dplyr::select(tidyselect::all_of(c(category_name, "estimate")))

  return(result)
}
