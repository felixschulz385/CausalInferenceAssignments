# Lab Assignment 2 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 18.04.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())

rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()

load("code/data_assignment.RData")

# Load necessary libraries
packages_vector <- c("haven", "dplyr", "tidyr", "sandwich", "expss",
                     "fBasics", "xtable", "data.table", "stargazer", "mfx", "jtools", 
                     "ggplot2")
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
data <- tibble(data)

# following slide 11 of set 3
calculate_atet <- function(data, outcome_var) {
  mean_d0_before <- mean(data[data$connected == 0 & data$submarines == 0, ][[outcome_var]], na.rm = TRUE)
  mean_d1_before <- mean(data[data$connected == 1 & data$submarines == 0, ][[outcome_var]], na.rm = TRUE)
  mean_d0_after <- mean(data[data$connected == 0 & data$submarines == 1, ][[outcome_var]], na.rm = TRUE)
  mean_d1_after <- mean(data[data$connected == 1 & data$submarines == 1, ][[outcome_var]], na.rm = TRUE)
  
  atet <- (mean_d1_after - mean_d1_before) - (mean_d0_after - mean_d0_before)
  return(atet)
}

atet <- calculate_atet(data, "employed")

bootstrap.np <- function(data, outcome_var, n_iter){
  obs <- nrow(data) # store the number of observations
  atet_boot <- rep_len(NA, n_iter) # empty matrix for storing effect estimates 
      
  # bootstrap loop
  for(i in 1:n_iter){ 
      
    # draw a bootstrap sample
    sboot<-sample(x=1:obs, # observations from which to choose 
                  size=obs, # number of obs as in original data
                  replace=TRUE) # with replacement (one obs allowed to appear more than once)
  
    # redefine y, d, m and x from the bootstrap sample    
    atet_boot[i] <- calculate_atet(data[sboot, ], outcome_var)

  }

  output <- list(sd=sd(atet_boot), distr=atet_boot)
  return(output)

}

# Set seed for replicability
set.seed(1234)
atet_sd <- bootstrap.np(data, "employed", 100)

# Export in table format
tibble(
  ATET = atet,
  SE = atet_sd$sd,
  `95% CI` = paste0("[", round(atet - 1.96 * atet_sd$sd, 3), ", ", round(atet + 1.96 * atet_sd$sd, 3), "]"),
) %>%
mutate(across(where(is.numeric), ~ round(., 4))) %>% # round all columns to 3 digits
  kableExtra::kbl(
    "latex", booktabs = TRUE, 
    caption = "Internet access and employment: Non-parametric bootstrap estimates of ATET",
    label = "tab:np_atet",
    ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  writeLines("output/tables/2_np_atet.tex")

#--------------------------------------------------------

# question (b) Parametric ATET, no fixed effects, no clustered SE
did1 <- feols(employed ~ connected + submarines + treatment, data)
vcov_did1 <- vcov(did1)

# question (c) Parametric ATET, no fixed effects, clustered SE
did2 <- feols(employed ~ connected + submarines + treatment, data)
vcov_did2 <- vcovCL(did2, cluster = ~ location)

# question (d) Parametric ATET, with fixed effects,clustered SE
did3 <- feols(employed ~ treatment | time + location, data)
vcov_did3 <- vcovCL(did3, cluster = ~ location)

# --------------------------------------------------------------
# Question 2: Adding Skilled to the regression
# --------------------------------------------------------------

did4 <- feols(employed ~ treatment + skilled | time + location, data)
vcov_did4 <- vcovCL(did4, cluster = ~ location)

# combine all in one table
texreg::texreg(list(did1, did2, did3, did4), 
                override.se = list(sqrt(diag(vcov_did1)),
                                   sqrt(diag(vcov_did2)),
                                   sqrt(diag(vcov_did3)),
                                   sqrt(diag(vcov_did4))),
                override.pvalues = list(2*pnorm(-abs(coef(did1)/sqrt(diag(vcov(did1))))),
                                        2*pnorm(-abs(coef(did2)/sqrt(diag(vcov_did2)))),
                                        2*pnorm(-abs(coef(did3)/sqrt(diag(vcov_did3)))),
                                        2*pnorm(-abs(coef(did4)/sqrt(diag(vcov_did4))))),
                custom.coef.names = c("Intercept", "Connected", "Submarines", "Treatment", "Skilled"),
                custom.model.names = c("(1b)", "(1c)", "(1d)", "(2)"),
                custom.gof.rows = list(
                  "Fixed Effects" = c("", "", "$\\checkmark$", "$\\checkmark$"),
                  "Clustered SEs" = c("", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")
                  ),
                include.rsquared = FALSE,
                include.adjrs = FALSE,
                include.nobs = TRUE,
                include.rmse = FALSE,
                caption = "Parametric ATET estimates",
                caption.above = TRUE,
                stars = c(0.001, 0.01, 0.05),
                single.row = FALSE,
                digits = 4,
                file = "output/tables/2_parametric_atet_combined.tex",
                )

# --------------------------------------------------------------
# Question 3: Fast Internet and Education
# --------------------------------------------------------------

# question (a): common trends plot

# create a group-means data set
common_trends <- data %>% 
                  group_by(time, connected) %>% 
                  summarise(educ_high_mean = mean(educ_high)) %>%
                  ungroup() %>%
                  mutate(connected = recode_factor(connected, 
                                            `0` = "Connected = 0",
                                            `1` = "Connected = 1"))

# plot
common_trends_plot <- ggplot(data = common_trends, 
       aes(x = time, y = educ_high_mean, 
           group = connected, 
           color = connected)) + 
  geom_line() +
  geom_vline(xintercept = 0.5, linetype="dashed") +
  scale_x_continuous(breaks = seq(-5, 4, by = 1)) +
  theme_bw(base_size = 20) +
  labs( y ="Share of highly educated", x= "Time", colour = "Group")

ggsave("output/figures/2_common_trends.png", common_trends_plot, width = 8, height = 4, dpi = 300)

# question (b): event study plot

event_study <- feols(educ_high ~ i(time, connected, 0) | time + location, data)

png(file="output/figures/2_event_study.png",width=6, height=3, units="in", res=300)
iplot(event_study)
dev.off()

# --------------------------------------------------------------
# Question 4: Clustered Bootstrap SEs
# --------------------------------------------------------------

bootstrap.cluster.np <- function(data, outcome_var, cluster_var, n_iter){
  clusters <- unique(data[[cluster_var]])
  atet_boot <- rep(NA, n_iter) # vector for storing effect estimates
      
  # bootstrap loop
  for(i in 1:n_iter){ 
      
    # draw a bootstrap sample of clusters
    sboot <- sample(x = clusters, 
                    size = length(clusters), 
                    replace = TRUE)

    # get data for the sampled clusters (with duplicates)
    data_iter <- purrr::map_df(sboot, ~ data[data[[cluster_var]] == ., ])
    
    # compute nonparametric ATET on the bootstrap sample
    atet_boot[i] <- calculate_atet(data_iter, outcome_var)
  }

  output <- list(sd = sd(atet_boot, na.rm = TRUE), distr = atet_boot)
  return(output)
}

# set seed for replicability
set.seed(1234)
atet_sd_boot <- bootstrap.cluster.np(data, "employed", "location", 100)

# export in table format
tibble(
  ATET = atet,
  SE = atet_sd_boot$sd,
  `95% CI` = paste0("[", round(atet - 1.96 * atet_sd_boot$sd, 3), ", ", round(atet + 1.96 * atet_sd_boot$sd, 3), "]"),
) %>%
mutate(across(where(is.numeric), ~ round(., 4))) %>% # round all columns to 3 digits
  kableExtra::kbl(
    "latex", booktabs = TRUE, 
    caption = "Internet access and employment: Non-parametric bootstrap estimates of ATET, Cluster-robust SEs",
    label = "tab:np_atet_clustered",
    ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  writeLines("output/tables/2_np_atet_cluster.tex")
