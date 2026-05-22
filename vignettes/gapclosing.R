## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 300, fig.width = 6.5, fig.height = 3, out.width = "650px")

## ----include = FALSE----------------------------------------------------------
t0 <- Sys.time()

## ----message = FALSE, warning = FALSE-----------------------------------------
set.seed(08544)
library(gapclosing)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
simulated_data <- generate_simulated_data(n = 1000)
head(simulated_data)

