# Lab Assignment 2 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 18.04.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())
load("code/data_assignment.RData")

# Load necessary libraries
packages_vector <- c("haven", "dplyr", "tidyr", "sandwich", "expss",
                     "fBasics", "xtable", "data.table", "stargazer", "mfx", "jtools", "ggplot2")
# install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE)
# DiD-specific packages
packaged_vector_did <- c("fixest")
# install.packages(packaged_vector_did)
lapply(packaged_vector_did, require, character.only = TRUE)


# --------------------------------------------------------------
# Question 1: Parametric and non parametric estimates of ATET
# --------------------------------------------------------------

attach(data)


# question (a) (code is copy-pasted from lab session)

balance_check.model <- function(x){
  # Conditional means
  mean_d0_before <- mean(x[data$connected==0 & data$submarines==0], na.rm=TRUE)
  mean_d1_before <- mean(x[data$connected==1 & data$submarines==0], na.rm=TRUE)
  mean_d0_after <- mean(x[data$connected==0 & data$submarines==1], na.rm=TRUE)
  mean_d1_after <- mean(x[data$connected==1 & data$submarines==1], na.rm=TRUE)
  # Difference in means before treatment
  diff_before <- lm(x[submarines==0] ~ data$connected[submarines==0])
  cov <- vcovHC(diff_before, type = "HC")
  robust.se_before <- sqrt(diag(cov))
  # Difference in means after treatment
  diff_after <- lm(x[submarines==1] ~ data$connected[submarines==1])
  cov <- vcovHC(diff_after, type = "HC")
  robust.se_after <- sqrt(diag(cov))
  list(mean_d0_before = mean_d0_before,
       mean_d1_before = mean_d1_before,
       diff_before = diff_before$coefficients[2],
       robust.se_before = robust.se_before[2],
       pval_before = 2*pnorm(-abs(diff_before$coefficients[2]/robust.se_before[2])),
       mean_d0_after = mean_d0_after,
       mean_d1_after = mean_d1_after,
       diff_after = diff_after$coefficients[2],
       robust.se_after = robust.se_after[2],
       pval_after = 2*pnorm(-abs(diff_after$coefficients[2]/robust.se_after[2]))
  )
}

outcomes<- dplyr::select(data, employed, skilled, hoursworked)
outcomes_names <- colnames(outcomes)


diff_output <- apply(outcomes, 2, balance_check.model)
# convert list to table
diff_output<-rbindlist(diff_output)
# add a row with number of observations
n_d0_before <- nrow(data[data$connected==0 & data$submarines==0,])
n_d1_before <- nrow(data[data$connected==1 & data$submarines==0,])
n_d0_after <- nrow(data[data$connected==0 & data$submarines==1,])
n_d1_after <- nrow(data[data$connected==1 & data$submarines==1,])
obs <-c(n_d0_before, n_d1_before, NA, NA, NA, n_d0_after, n_d1_after, NA, NA, NA)

diff_output <- rbind(as.matrix(diff_output), obs)
rownames(diff_output)<- c(outcomes_names, "Observations")
colnames(diff_output)<- c("E(Y|D=0, T=0)", "E(Y|D=1, T=0)",
                          "Difference", "s.e.", "p-value",
                          "E(Y|D=0, T=1)", "E(Y|D=1, T=1)",
                          "Difference", "s.e.", "p-value")
print(round(diff_output, digits=2))


# I think the estimates are the correct ones but I'm not use how to do the boostrapping here

# question (b) Parametric ATET, no fixed effects, no clustered SE
did1 <- feols(employed ~ connected + submarines + treatment, data)
did1 <- summary(did1)
did1

# question (c) Parametric ATET, no fixed effects, clustered SE
did2 <- feols(employed ~ connected + submarines + treatment, data)
did2 <- summary(did2, cluster = "location")
did2

# question (d) Parametric ATET, with fixed effects,clustered SE
did3 <- feols(employed ~ treatment | time + location, data)
did3 <- summary(did3, cluster = "location")
did3


# --------------------------------------------------------------
# Question 2: Adding Skilled to the regression
# --------------------------------------------------------------

# question (b)
did4 <- feols(employed ~ treatment + skilled | time + location, data)
did4 <- summary(did4, cluster = "location")
did4











