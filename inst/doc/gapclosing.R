## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 300, fig.width = 6.5, fig.height = 3, out.width = "650px")

## ---- include = F-------------------------------------------------------------
t0 <- Sys.time()

## ---- message = F, warning = F------------------------------------------------
set.seed(08544)
library(gapclosing)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
simulated_data <- generate_simulated_data(n = 1000)
head(simulated_data)

## -----------------------------------------------------------------------------
estimate <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  outcome_formula = formula(outcome ~ confounder + category*treatment),
  treatment_formula = formula(treatment ~ confounder + category),
  category_name = "category",
  se = T,
  # Process bootstrap in parallel with as many cores as available
  parallel_cores = 2
)

## ---- fig.width = 5, fig.height = 3.5, out.width = "650px", fig.cap = "Figure 1 produced by plot() function"----
plots <- plot(estimate, return_plots = TRUE)
print(plots[[1]])

## ---- fig.width = 5, fig.height = 3.5, out.width = "650px", fig.cap = "Figure 1 produced by plot() function"----
print(plots[[3]])

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 300)
summary(estimate)

## ---- echo = F, fig.height = 2----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for_DAG <- data.frame(label = c("Category","Confounder","Treatment","Outcome","Unobserved"),
                      x = c(1,2,3,4,1),
                      y = c(1,1,1,1,0))
for_DAG %>%
  ggplot(aes(x = x, y = y, label = label)) +
  # Edges coming out of category
  annotate(geom = "segment",
         x = 1, y = 1, xend = 1.55, yend = 1,
         arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "curve",
           x = 1, y = 1, xend = c(2.9,3.9), yend = c(1.2,1.2),
           arrow = arrow(length = unit(.1,"in")),
           curvature = -.3) +
  # Edges coming out of confounder
  annotate(geom = "segment",
           x = 2, y = 1, xend = 2.65, yend = 1,
           arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "curve",
           x = 2, y = 1, xend = 3.75, yend = .82,
           arrow = arrow(length = unit(.1,"in")),
           curvature = .25) +
  # Edges coming out of treatment
  annotate(geom = "segment",
           x = 3, y = 1, xend = 3.65, yend = 1,
           color = "blue", size = 1.05,
           arrow = arrow(length = unit(.13,"in"))) +
  # Edges coming out of unobserved
  annotate(geom = "segment",
           x = 1, y = 0, xend = c(1,1.9), yend = .8,
           arrow = arrow(length = unit(.1,"in"))) +
  annotate(geom = "curve",
           x = 1, y = 0, xend = c(3.9), yend = .8,
           arrow = arrow(length = unit(.1,"in")),
           curvature = .25) +
  geom_label(color = "white") +
  geom_label(data = for_DAG %>% filter(label %in% c("Category","Confounder"))) +
  geom_text(data = for_DAG %>% filter(label %in% c("Unobserved","Treatment","Outcome"))) +
  ylim(c(-.3,2)) +
  xlim(c(0,5)) +
  theme_void()

## ---- warning = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimate_gam <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  outcome_formula = formula(outcome ~ s(confounder) + category*treatment),
  treatment_formula = formula(treatment ~ s(confounder) + category),
  category_name = "category",
  treatment_algorithm = "gam",
  outcome_algorithm = "gam",
  sample_split = "cross_fit",
  se = T,
  parallel_cores = 2
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimate_ranger <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  outcome_formula = formula(outcome ~ confounder + category),
  treatment_formula = formula(treatment ~ confounder + category),
  category_name = "category",
  treatment_algorithm = "ranger",
  outcome_algorithm = "ranger",
  sample_split = "cross_fit",
  se = T,
  parallel_cores = 2
)

## ---- include = F-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glm_plot <- plot(estimate, return_plots = T)[[1]]
gam_plot <- plot(estimate_gam, return_plots = T)[[1]]
ranger_plot <- plot(estimate_ranger, return_plots = T)[[1]]

## ---- echo = F, fig.width = 12, fig.height = 5, message = F, warning = F, fig.caption = "The fourth default plot() with minor modifications, using results from each estimator above."--------------------------------------------------------------------------------------------------------------------
gridExtra::grid.arrange(
  glm_plot +
    ggtitle("GLM estimate") +
    ylim(c(-1.5,1.5)),
  gam_plot +
    ggtitle("GAM estimate") +
    ylim(c(-1.5,1.5)),
  ranger_plot +
    ggtitle("Ranger estimate") +
    ylim(c(-1.5,1.5)),
  ncol = 3
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estimate_stochastic <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = .75,
  outcome_formula = formula(outcome ~ confounder + category*treatment),
  treatment_formula = formula(treatment ~ confounder + category),
  category_name = "category"
)

## ---- echo = F, fig.caption = "A conceptual figure for which the package does not provide a general-purpose function."------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
counterfactual_assignments_values <- seq(0,1,.25)
many_stochastic_estimates <- rep(NA, length(counterfactual_assignments_values))
for (i in 1:length(counterfactual_assignments_values)) {
  estimate_case <- gapclosing(
    data = simulated_data,
    counterfactual_assignments = counterfactual_assignments_values[i],
    outcome_formula = formula(outcome ~ confounder + category*treatment),
    treatment_formula = formula(treatment ~ confounder + category),
    category_name = "category"
  )
  estimate_we_want <- estimate_case$change_disparities %>%
    filter(change_type == "proportional") %>%
    filter(category == "B - A")
  many_stochastic_estimates[i] <- estimate_we_want$estimate
}
data.frame(counterfactual_assignments = counterfactual_assignments_values,
           estimate = many_stochastic_estimates) %>%
  ggplot(aes(x = counterfactual_assignments,
             y = estimate)) +
  geom_hline(yintercept = c(0,1),
             color = "gray", linetype = "dashed") +
  geom_line(color = "gray") +
  geom_point(aes(color = factor(counterfactual_assignments == .75))) +
  xlab("Counterfactual Treatment Assignment Probability") +
  scale_y_continuous(name = "Percent of B - A Gap Closed",
                     labels = function(x) paste0(round(100*x),"%")) +
  annotate(geom = "text",
           x = 1, y = c(0,1),
           label = c("No change to disparity",
                     "Disparity completely eliminated"),
           color = "darkgray", size = 3, hjust = 1, vjust = 1.5) +
  annotate(geom = "text",
           x = .1, y = .25, 
           label = "Disparities eliminated under various\nstochastic\ninterventions",
           color = "gray", size = 3, hjust = 0, vjust = 1) +
  annotate(geom = "text",
           x = .75, y = .3,
           label = "Gap-Closing Estimand\nCalculated Above",
           color = "blue", size = 3, vjust = -.5) +
  scale_color_manual(values = c("gray","blue")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
our_assignments <- case_when(simulated_data$category == "A" ~ .5,
                             simulated_data$category == "B" ~ .4, 
                             simulated_data$category == "C" ~ .3)
estimate_stochastic <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = our_assignments,
  outcome_formula = formula(outcome ~ confounder + category*treatment),
  treatment_formula = formula(treatment ~ confounder + category),
  category_name = "category"
)

