#' Generate simulated data
#' @description Generates simulated data to illustrate the gapclosing function
#' @param n Number of observations to be generated
#' @return A data frame with \code{n} rows and 4 columns containing simulated data containing \code{category} over which disparities are defined, a \code{confounder} that affects treatment assignment, a binary \code{treatment}, and a continuous \code{outcome}.
#' @references Lundberg, Ian. 2021. "The gap-closing estimand: A causal approach to study interventions that close disparities across social categories." {https://osf.io/gx4y3/}
#' @export

generate_simulated_data <- function(n = 1000) {
  data.frame(category = sample(c("A","B","C"), n, replace = T)) %>%
    # Create a confounding variable called confounder
    mutate(confounder = rnorm(n,
                              mean = case_when(category == "A" ~ -1.5,
                                               category == "B" ~ 0,
                                               category == "C" ~ 1.5),
                              sd = 1),
           # Assign treatment as a function of the confounder
           treatment = rbinom(n, size = 1, prob = plogis(confounder)),
           # Create a treatment effect that varies by category
           treatment_effect = case_when(category == "A" ~ 1.3,
                                        category == "B" ~ 0,
                                        category == "C" ~ -1.2),
           # Generate the outcome
           outcome = rnorm(n, mean = confounder + treatment*treatment_effect, sd = 1)) %>%
    # Remove the treatment effect, which one would not know in reality
    select(-treatment_effect)
}
