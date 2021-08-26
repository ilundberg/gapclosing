#' Plot function for gapclosing objects
#' @description Produces summary plots for a gapclosing object.
#' @param x An object of class \code{gapclosing}, which results from a call to the function \code{gapclosing}
#' @param return_plots Logical, defaults to FALSE. If TRUE, returns a list of the 4 plots without printing. Defaults to FALSE, in which case the console will interactively ask the user to hit "return" to proceed through printouts of the four plots, with no plots returned.
#' @param arranged Logical, defaults to FALSE. If TRUE, returns a list of the 4 plots arranged in a 2x2 table. Useful to visualize all four in one screen.
#' @param ... Other arguments to \code{plot} commands
#' @return If \code{return_plots = TRUE}, returns a list of length 4, each element of which is a ggplot2 object. If \code{return_plots = FALSE} (the default), then nothing is returned.
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @importFrom foreach %do%
#' @importFrom magrittr %>%
#' @export

plot.gapclosing <- function(x, return_plots = F, arranged = F, ...) {

  # Prepare for non-standard evaluation
  setting <- gapclosing.category <- estimate <- ci.min <- ci.max <- NULL

  mean_outcomes_data <- x$factual_means %>%
    dplyr::mutate(setting = "Factual") %>%
    dplyr::bind_rows(x$counterfactual_means %>%
                       dplyr::mutate(setting = "Counterfactual")) %>%
    dplyr::mutate(setting = forcats::fct_rev(setting))

  mean_outcomes <- mean_outcomes_data %>%
    dplyr::rename_with(function(this_name) dplyr::case_when(this_name == x$arguments$category_name ~ "gapclosing.category",
                                                            T ~ this_name)) %>%
    ggplot2::ggplot(ggplot2::aes(x = gapclosing.category, y = estimate,
                                 color = setting, shape = setting)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .2)) +
    ggplot2::theme_bw() +
    ggplot2::ylab("Mean Outcome") +
    ggplot2::xlab("Category") +
    ggplot2::theme(legend.title = ggplot2::element_blank())

  # If there was a standard error, add the CI to the plot
  if (x$arguments$se) {
    mean_outcomes <- mean_outcomes +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.min, ymax = ci.max),
                             width = .1,
                             position = ggplot2::position_dodge(width = .2))
  }


  unique_categories <- unique(x$factual_means[[x$arguments$category_name]])
  num_unique_categories <- length(unique_categories)
  category_pairs <- matrix(NA, nrow = 0, ncol = 2, dimnames = list(NULL, c("category_A","category_B")))
  for (i in 1:(num_unique_categories - 1)) {
    category_pairs <- rbind(category_pairs,
                            cbind(unique_categories[i],
                                  unique_categories[(i+1):num_unique_categories]))
  }

  pairwise_plots <- lapply(1:nrow(category_pairs), function(i) {
    disparityplot(x, category_pairs[i,1], category_pairs[i,2])
  })
  names(pairwise_plots) <- apply(category_pairs,1, function(value) paste0("gap_",value[1],"_",value[2]))

  if (!return_plots) {
    # Print the plots to the viewer
    print(mean_outcomes)
    for (i in 1:length(pairwise_plots)) {
      readline(prompt = "Press return to see the next plot")
      print(pairwise_plots[[i]])
    }
    print("If you would prefer to save these plots, use the argument return_plots = TRUE")
  } else {
    # If requested, return the plots as a list
    print("Returning a list containing a series of plots")
    return(c(mean_outcomes = list(mean_outcomes), pairwise_plots))
  }
}
