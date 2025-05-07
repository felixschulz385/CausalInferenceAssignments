# Lab Assignment 4 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 16.05.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())

#Load Packages
# Define packages that you need,
packages_vector <- c("haven", "dplyr", "tidyr", "sandwich",
                     "expss", "fBasics", "xtable", "data.table",
                     "stargazer", "mfx", "jtools", "ggplot2")
# install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE)
# RDD-specific packages
packaged_vector_rdd <- c("lpdensity", # Local Polynomial Density Estimation and Inference
                         "rddensity", # Manipulation Testing Based on Density Discontinuity
                         "rdlocrand", # Local Randomization Methods for RD Designs
                         "rdrobust", # Robust Data-Driven Statistical Inference in RDD
                         "rdd") #Regression Discontinuity Estimation
# install.packages(packaged_vector_rdd)
lapply(packaged_vector_rdd, require, character.only = TRUE)


load("code/meyersson_RDD2.RData")

# Question 3

generate_simulated_data <- function(sample_size) {
  set.seed(123)
  # Generate error term 
  e_sim <- rnorm(sample_size, mean = 0, sd = 50)
  # Generate running variable 
  X_sim <- runif(sample_size, min = -50, max = 50)
  # Makes a data frame
  sim_data <- data.frame(X_sim = X_sim, e_sim = e_sim) 
  # Add remaining variables to dataframe
  D_sim <- ifelse(sim_data$X_sim >0, 1, 0)
  sim_data$D_sim <- D_sim
  
  DX_sim <- X_sim * D_sim
  sim_data$DX_sim <- DX_sim
  
  Y_sim <- 2-5*D_sim + 0.5*X_sim - DX_sim + e_sim
  sim_data$Y_sim <- Y_sim
  
  model_rdd <- rdrobust(sim_data$Y_sim, sim_data$X_sim, c=0, p=1, 
                    kernel='triangular', bwselect='mserd')
  model_ols <- lm(Y_sim ~ D_sim + X_sim + DX_sim, data = sim_data)
  cov <- vcovHC(model_ols, type = "HC")
  robust.se <- sqrt(diag(cov))
  
  
  return(list(sim_data = sim_data, model_rdd = model_rdd, 
              model_ols = model_ols, robust.se = robust.se))
}

###################### Trying out the function ######################
result <- generate_simulated_data(10000)

sim_data <- result$sim_data
sim_model_rdd <- result$model_rdd
sim_model_ols <- result$model_ols
sim_robust.se <- result$robust.se


summary(sim_model_rdd)
summary(sim_model_ols)
#####################################################################

# Sample sizes to run
sample_sizes <- c(5000, 10000, 20000)

# Storage for results
results_list <- lapply(sample_sizes, generate_simulated_data)

# Create table using tibble
stats <- tibble(
  "Sample" = sample_sizes,
  "Estimate" = sapply(results_list, \(res) round(res$model_rdd$coef[1], 2)),
  "SE"       = sapply(results_list, \(res) round(res$model_rdd$se[1], 2)),
  "BW"       = sapply(results_list, \(res) round(res$model_rdd$bws[1, 1], 2)),
  "D Estimate"  = sapply(results_list, \(res) round(coef(res$model_ols)["D_sim"], 2)),
  "D SE"        = sapply(results_list, \(res) round(res$robust.se["D_sim"], 2)),
  "X Estimate"  = sapply(results_list, \(res) round(coef(res$model_ols)["X_sim"], 2)),
  "X SE"        = sapply(results_list, \(res) round(res$robust.se["X_sim"], 2)),
  "DX Estimate" = sapply(results_list, \(res) round(coef(res$model_ols)["DX_sim"], 2)),
  "DX SE"       = sapply(results_list, \(res) round(res$robust.se["DX_sim"], 2))
)

# Create LaTeX table and write to file
stats %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Simulation Results for Different Sample Sizes",
    label = "tab:sim_results",
    digits = 2,
    escape = FALSE,
    align = "c"
  ) %>%
  add_header_above(c(" " = 1, "RD" = 3, "OLS" = 6)) %>%
  writeLines("output/tables/4_sim_results_table.tex")



