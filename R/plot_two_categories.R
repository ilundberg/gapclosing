
#' Plot gap closing two categories
#' @description Plots the factual and counterfactual mean outcomes in two categories. The returned object is a ggplot2 object which can be further customized using the syntax of ggplot2.
#' @param x An object of class \code{gapclosing}, which results from a call to the function \code{gapclosing}
#' @param category_A The first category to be plotted. Must be a value of \code{x$category}
#' @param category_B The second category to be plotted. Must be a value of \code{x$category}
#' @param custom_ylab Custom y-axis label. Defaults to "Mean Outcome"
#' @param custom_xlab Custom x-axis label. Defaults to "Category"
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @export

plot_two_categories <- function(x, category_A, category_B, custom_ylab = "Mean Outcome", custom_xlab = "Category") {
  # Initialize non-standard evaluation variables to avoid R CMD check warnings.
  setting <- category <- estimate <- se <- label <- y <- Factual <- Counterfactual <- NULL
  # If the estimated thing is the reverse (e.g. B - A instead of A - B), then create the one the user wants.
  case_in_x <- any(x$primary_estimate$category == paste(category_A,"-",category_B))
  if (!case_in_x) {
    x$primary_estimate$estimate[x$primary_estimate$category == paste(category_B,"-",category_A)] <-
      -x$primary_estimate$estimate[x$primary_estimate$category == paste(category_B,"-",category_A)]
    x$primary_estimate$category[x$primary_estimate$category == paste(category_B,"-",category_A)] <-
      paste(category_A,"-",category_B)
  }
  # Create data frame for plot
  forplot <- x$primary_estimate %>%
    dplyr::filter(setting %in% c("factual","counterfactual") &
                    category %in% c(category_A, category_B,paste(category_A,"-",category_B))) %>%
    # Place categories in the user-specified order
    dplyr::mutate(category = forcats::fct_relevel(category, category_A, category_B),
                  setting = dplyr::case_when(setting == "factual" ~ "Factual",
                                             setting == "counterfactual" ~ "Counterfactual"),
                  setting = forcats::fct_rev(setting))
  means <- forplot %>%
    dplyr::filter(!grepl("-",category))
  disparities <- forplot %>%
    dplyr::filter(grepl("-",category))

  plot <- means %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(color = setting, shape = setting,
                                     x = category, y = estimate),
                        position = ggplot2::position_dodge(width = .1)) +
    ggplot2::geom_errorbar(ggplot2::aes(color = setting,
                                        x = category,
                                        ymin = estimate - stats::qnorm(.975) * se,
                                        ymax = estimate + stats::qnorm(.975) * se),
                           width = .1, size = .3,
                           position = ggplot2::position_dodge(width = .1)) +
    ggplot2::geom_line(ggplot2::aes(color = setting,
                                    x = ifelse(setting == "Factual", 1.25, 1.75),
                                    y = estimate),
                       position = ggplot2::position_dodge(width = .1),
                       size = .5) +
    ggplot2::geom_segment(ggplot2::aes(color = setting,
                                       x = category, xend = ifelse(setting == "Factual", 1.25, 1.75),
                                       y = estimate, yend = estimate),
                          linetype = "dashed",
                          position = ggplot2::position_dodge(width = .1),
                          size = .5) +
    ggplot2::geom_text(data = disparities,
                       ggplot2::aes(color = setting,
                                    x = ifelse(setting == "Factual", 1.25, 1.75),
                                    label = paste0(ifelse(setting == "Factual","Descriptive\nDisparity","Gap Closing\nEstimand")),
                                    y = max(means$estimate) + .2 * (max(means$estimate) - min(means$estimate))),
                       vjust = 0,
                       size = 3,
                       show.legend = F) +
    ggplot2::geom_text(data = means %>%
                         dplyr::group_by(setting) %>%
                         dplyr::summarize(label = format(abs(diff(estimate)), digits = 2),
                                          y = mean(estimate)),
                       ggplot2::aes(color = setting,
                                    x = ifelse(setting == "Factual", 1.25, 1.75),
                                    label = label,
                                    y = y, hjust = ifelse(setting == "Factual", -.2, 1.2)),
                       position = ggplot2::position_dodge(width = .1),
                       size = 3,
                       show.legend = F) +
    # Note the treatment effects
    ggplot2::geom_segment(data = means %>%
                            dplyr::select(setting, category, estimate) %>%
                            tidyr::spread(key = setting, value = estimate) %>%
                            dplyr::mutate(x = ifelse(category == category_A, .8, 2.2)),
                          ggplot2::aes(x = x, xend = x, y = Factual, yend = Counterfactual),
                          arrow = grid::arrow(length = grid::unit(.1,"in")),
                          color = "gray") +
    ggplot2::geom_text(data = means %>%
                         dplyr::group_by(category) %>%
                         dplyr::summarize(y = mean(estimate),
                                          x = ifelse(category == category_A, .8, 2.2),
                                          .groups = "drop"),
                       ggplot2::aes(x = x, y = y, vjust = ifelse(category == category_A,-1,2)),
                       label = "Causal Effect", color = "gray",
                       size = 2, angle = 90) +
    # Edit scale appearances
    ggplot2::scale_x_discrete(name = custom_xlab,
                              expand = ggplot2::expansion(add = .35)) +
    ggplot2::scale_y_continuous(name = custom_ylab,
                                expand = ggplot2::expansion(mult = .2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right",
                   legend.title = ggplot2::element_blank())
  return(plot)
}
