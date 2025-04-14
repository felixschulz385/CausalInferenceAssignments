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
data <- tibble(data)


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

## Felix' solution
# following slide 11 of set 3

calculate_atet <- function(data, outcome_var) {
  mean_d0_before <- mean(data[data$connected == 0 & data$submarines == 0, ][[outcome_var]], na.rm = TRUE)
  mean_d1_before <- mean(data[data$connected == 1 & data$submarines == 0, ][[outcome_var]], na.rm = TRUE)
  mean_d0_after <- mean(data[data$connected == 0 & data$submarines == 1, ][[outcome_var]], na.rm = TRUE)
  mean_d1_after <- mean(data[data$connected == 1 & data$submarines == 1, ][[outcome_var]], na.rm = TRUE)
  
  atet <- mean_d1_after - mean_d1_before - (mean_d0_after - mean_d0_before)
  return(atet)
}

# Example usage:
atet <- calculate_atet(data, "employed")

bootstrap.np <- function(data, outcome_var, n_iter){
  obs <- nrow(data) # store the number of observations
  atet_boot <- rep_len(NA, n_iter) # empty matrix for storing effect estimates 
      
  # The bootstrap loop starts here:
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

atet_sd <- bootstrap.np(data, "employed", 1000)

# Export in table format
tibble(
  ATET = atet,
  SD = atet_sd$sd,
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

# Create a group-means data set
common_trends <- data %>% 
                  group_by(time, connected) %>% 
                  summarise(educ_high_mean = mean(educ_high)) %>%
                  ungroup() %>%
                  mutate(connected = recode_factor(connected, 
                                            `0` = "Connected = 0",
                                            `1` = "Connected = 1"))

# Plot
common_trends_plot <- ggplot(data = common_trends, 
       aes(x = time, y = educ_high_mean, 
           group = connected, 
           color = connected)) + 
  geom_line() +
  geom_vline(xintercept = 0.5, linetype="dashed") +
  scale_x_continuous(breaks = seq(-5, 4, by = 1)) +
  theme_bw(base_size = 20) +
  labs( y ="Share of highly educated", x= "Time", colour = "Group")

ggsave("output/figures/2_common_trends.png", common_trends_plot, width = 10, height = 6, dpi = 300)

# question (b): event study plot

event_study <- feols(educ_high ~ i(time, connected, 0) | time + location, data)

png(file="output/figures/2_event_study.png",width=10, height=6, units="in", res=300)
iplot(event_study)
dev.off()

# --------------------------------------------------------------
# Question 4: Clustered Bootstrap SEs
# --------------------------------------------------------------

bootstrap.cluster.np <- function(data, outcome_var, cluster_var, n_iter){
  clusters <- unique(data[[cluster_var]])
  atet_boot <- rep(NA, n_iter) # vector for storing effect estimates 
      
  # Bootstrap loop
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


atet_sd_boot <- bootstrap.cluster.np(data, "employed", "location", 100)

# Export in table format
tibble(
  ATET = atet,
  SD = atet_sd_boot$sd,
  `Lower Bound` = atet - 1.96 * atet_sd_boot$sd,
  `Upper Bound` = atet + 1.96 * atet_sd_boot$sd
) %>%
mutate(across(everything(), ~ round(., 3))) %>% # round all columns to 3 digits
  kableExtra::kbl(
    "latex", booktabs = TRUE, 
    caption = "Internet access and employment: Non-parametric bootstrap estimates of ATET, Cluster-robust SEs",
    label = "tab:np_atet_clustered",
    ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  writeLines("output/tables/2_np_atet_cluster.tex")
