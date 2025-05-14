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
                     "stargazer", "mfx", "jtools", "ggplot2", 
                     "kableExtra")
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

# --------------------------------------------------------------
# Question 1
# --------------------------------------------------------------

# question (a)

below_income <- meyersson.data %>% 
  filter(anhinc99 < median(anhinc99, na.rm = TRUE)) %>%
  tibble()

above_income <- meyersson.data %>% 
  filter(anhinc99 >= median(anhinc99, na.rm = TRUE)) %>%
  tibble()

rdd_below <- rdrobust(
  below_income$Y, below_income$X,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

rdd_above <- rdrobust(
  above_income$Y, above_income$X,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

# question (b)

COVS_below <- COVS[meyersson.data$anhinc99 < median(meyersson.data$anhinc99, na.rm = TRUE), ]
COVS_above <- COVS[meyersson.data$anhinc99 >= median(meyersson.data$anhinc99, na.rm = TRUE), ]

rdd_below_covs <- rdrobust(
  below_income$Y, below_income$X, covs = COVS_below,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

rdd_above_covs <- rdrobust(
  above_income$Y, above_income$X, covs = COVS_above,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

# question (d)

rdd_test <- rdrobust(
  meyersson.data$anhinc99, meyersson.data$X,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

# ggplot(meyersson.data) +
#   geom_jitter(aes(x = X, y = anhinc99), alpha = 0.5)

# output table of results (coef, se)
effects <- matrix(NA, nrow = 2, ncol = 6) # Changed from 4 to 6 columns
effects[1,1] <- rdd_below$Estimate[[1]]
effects[1,2] <- rdd_below$se[[1]]
effects[1,3] <- rdd_above$Estimate[[1]]
effects[1,4] <- rdd_above$se[[1]]
effects[1,5] <- rdd_test$Estimate[[1]]  # Added test columns
effects[1,6] <- rdd_test$se[[1]]        # Added test columns
effects[2,1] <- rdd_below_covs$Estimate[[1]]
effects[2,2] <- rdd_below_covs$se[[1]]
effects[2,3] <- rdd_above_covs$Estimate[[1]]
effects[2,4] <- rdd_above_covs$se[[1]]
effects[2,5] <- NA  # No test with covariates
effects[2,6] <- NA  # No test with covariates
effects <- round(effects, 2)
effects <- as.data.frame(effects)
effects <- bind_cols(c("Without covariates", "With covariates"), effects)
colnames(effects) <- c("", "Estimate", "SE", "Estimate", "SE", "Estimate", "SE")

# output to LaTeX
effects %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "RDD Estimates for Below and Above Median Income",
    label = "tab:rdd_results",
    digits = 2,
    escape = FALSE,
    align = "c"
  ) %>%
  add_header_above(c(" " = 1, "Below Median" = 2, "Above Median" = 2, "RDD Test" = 2)) %>%
  writeLines("output/tables/4_rdd_results.tex")

# --------------------------------------------------------------
# Question 2
# --------------------------------------------------------------

# question (a)

pdf("output/figures/4_density_test.pdf", width = 8, height = 6)
mccrary <- DCdensity(
  meyersson.data$X, 
  cutpoint=0, ext.out=TRUE,
  plot=TRUE, verbose=FALSE)
dev.off()

paste0(
    "The estimated log difference in heights at the cutoff is ",
    round(mccrary$theta, 2),
    " with a standard error of ",
    round(mccrary$se, 2),
    ". The p-value is ",
    sprintf("%.2f", mccrary$p),
    "."
    ) %>%
    writeLines("output/other/4_mccrary_test.txt")


# question (b)

# Run RD estimations for age share variables
rdd_ageshr60 <- rdrobust(
  meyersson.data$ageshr60, meyersson.data$X,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

rdd_ageshr19 <- rdrobust(
  meyersson.data$ageshr19, meyersson.data$X,
  p = 1,
  kernel = "triangular",
  bwselect = "mserd"
)

# Create a table of results
age_effects <- matrix(NA, nrow = 2, ncol = 3)
age_effects[1,1] <- rdd_ageshr60$Estimate[[1]]
age_effects[1,2] <- rdd_ageshr60$se[[1]]
age_effects[1,3] <- rdd_ageshr60$pv[[1]]
age_effects[2,1] <- rdd_ageshr19$Estimate[[1]]
age_effects[2,2] <- rdd_ageshr19$se[[1]]
age_effects[2,3] <- rdd_ageshr19$pv[[1]]
age_effects <- round(age_effects, 3)
age_effects <- as.data.frame(age_effects)
age_effects <- bind_cols(c("Age share above 60", "Age share below 19"), age_effects)
colnames(age_effects) <- c("Variable", "Estimate", "SE", "p-value")

# Output to LaTeX
age_effects %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "RDD Estimates for Age Demographics",
    label = "tab:age_demographics",
    digits = 3,
    escape = FALSE,
    align = "c"
  ) %>%
  writeLines("output/tables/4_age_demographics.tex")


# --------------------------------------------------------------
# Question 3
# --------------------------------------------------------------

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



