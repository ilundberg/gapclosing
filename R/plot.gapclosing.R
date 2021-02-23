#' Plot function for gapclosing objects
#' @description Produces summary plots for a gapclosing object.
#' @param x An object of class \code{gapclosing}, which results from a call to the function \code{gapclosing}
#' @param return_plots Logical, defaults to FALSE. If TRUE, returns a list of the 4 plots without printing. Defaults to FALSE, in which case the console will interactively ask the user to hit "return" to proceed through printouts of the four plots, with no plots returned.
#' @param arranged Logical, defaults to FALSE. If TRUE, returns a list of the 4 plots arranged in a 2x2 table. Useful to visualize all four in one screen.
#' @param ... Other arguments to \code{plot} commands
#' @return If \code{return_plots = TRUE}, returns a list of length 4, each element of which is a ggplot2 object. If \code{return_plots = FALSE} (the default), then nothing is returned.
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @export

plot.gapclosing <- function(x, return_plots = F, arranged = F, ...) {
  # Initialize non-standard evaluation variables to avoid R CMD check warnings.
  description <- category <- estimate <- ci.min <- ci.max <- NULL
  plot_1 <- x$primary_estimate %>%
    dplyr::filter(description %in% c("Factual mean","Counterfactual mean")) %>%
    dplyr::mutate(description = dplyr::case_when(setting == "factual" ~ "Desriptive mean:\nMean of outcomes as\nthey factually exist",
                                                 setting == "counterfactual" ~ "Causal post-intervention mean:\nMean of potential outcomes\nunder the intervention")) %>%
    dplyr::mutate(description = forcats::fct_rev(description)) %>%
    ggplot2::ggplot(ggplot2::aes(x = category, y = estimate,
                                 color = description, shape = description)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::xlab("Category of Units") +
    ggplot2::ylab("\nEstimated Mean Outcome") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.key.height = grid::unit(36,"pt"),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::ggtitle("How the intervention would change mean outcomes")
  # If standard errors were calculated, add the confidence interval
  if (x$call$se) {
    plot_1 <- plot_1 +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.min, ymax = ci.max),
                             position = ggplot2::position_dodge(width = .5),
                             width = .2)
  }

  plot_2 <- x$primary_estimate %>%
    dplyr::filter(description %in% c("Factual gap","Counterfactual gap")) %>%
    dplyr::mutate(description = dplyr::case_when(setting == "factual" ~ "Desriptive disparity:\nDisparity in outcomes as\nthey factually exist",
                                                 setting == "counterfactual" ~ "Causal gap-closing estimand:\nDisparity in potential outcomes\nunder the intervention")) %>%
    dplyr::mutate(description = forcats::fct_rev(description)) %>%
    ggplot2::ggplot(ggplot2::aes(x = category, y = estimate,
                                 color = description, shape = description)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::xlab("Disparity Defined Across These Categories") +
    ggplot2::ylab("Estimated Disparity\nin Mean Outcomes") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.key.height = grid::unit(36,"pt"),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::ggtitle("Disparities with and without the intervention")
  # If standard errors were calculated, add the confidence interval
  if (x$call$se) {
    plot_2 <- plot_2 +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.min, ymax = ci.max),
                             position = ggplot2::position_dodge(width = .5),
                             width = .2)
  }

  plot_3 <- x$primary_estimate %>%
    dplyr::filter(description == "Amount of gap closed: Factual - Counterfactual") %>%
    ggplot2::ggplot(ggplot2::aes(x = category, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::xlab("Disparity Defined Across These Categories") +
    ggplot2::ylab("Factual Disparity -\nCounterfactual Disparity") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Amount of the gap closed by the intervention")
  # If standard errors were calculated, add the confidence interval
  if (x$call$se) {
    plot_3 <- plot_3 +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.min, ymax = ci.max),
                             position = ggplot2::position_dodge(width = .5),
                             width = .2)
  }

  plot_4 <- x$primary_estimate %>%
    dplyr::filter(description == "Proportion of gap closed: (Factual - Counterfactual) / Factual") %>%
    ggplot2::ggplot(ggplot2::aes(x = category, y = estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .5)) +
    ggplot2::xlab("Disparity Defined Across These Categories") +
    ggplot2::ylab(bquote(frac('Factual - Counterfactual','Factual'))) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100*x),"%")) +
    ggplot2::ggtitle("Percent of gap closed by the intervention")
  # If standard errors were calculated, add the confidence interval
  if (x$call$se) {
    plot_4 <- plot_4 +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ci.min, ymax = ci.max),
                             position = ggplot2::position_dodge(width = .5),
                             width = .2)
  }

  if (!return_plots & !arranged) {
    print("There will be 4 plots.")
    print("If you instead prefer to return a list of all 4 plots, use argument return_plots = T")
    print(plot_1)
    readline(prompt = "Press return to see the next plot")
    print(plot_2)
    readline(prompt = "Press return to see the next plot")
    print(plot_3)
    readline(prompt = "Press return to see the next plot")
    print(plot_4)
  }
  if (arranged) {
    return(gridExtra::grid.arrange(plot_1,plot_2,plot_3,plot_4,
                                   heights = c(1.3,1)))
  } else if (return_plots) {
    return(list(plot_1,plot_2,plot_3,plot_4))
  }
}
