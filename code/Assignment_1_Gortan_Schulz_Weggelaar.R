# Lab Assignment 1 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 04.04.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())
load("code/Assignment_1.RData")

# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)

# Turn into tibble
raw.data <- tibble(raw.data)

# --------------------------------------------------------------
# Question 1: Balancedness by Age
# --------------------------------------------------------------

# Create dummy variable for below 40
raw.data$below40 <- ((raw.data$agegr_2 == 1) | (!raw.data$agegr_2 & !raw.data$agegr_3 & !raw.data$agegr_4)) * 1

# Create factor variables
raw.data$treat_f <- factor(raw.data$treat, levels = c(0,1), label = c("D=0", "D=1")) 
raw.data$below40_f <- factor(raw.data$below40, levels = c(0,1), label = c("Age ≥ 40", "Age < 40"))

# Compute share by treatment status
raw.data %>%
    group_by(treat_f) %>%
    summarize(mean(below40)) %>%
    rename(
        "$Share \\leq 40$" = "mean(below40)",
        "Treatment Status" = "treat_f"
        ) %>%
    kableExtra::kable("latex", escape = F, booktabs = TRUE, digits = 3, caption = "Share of Age Groups by Treatment Status") %>%
    kableExtra::kable_styling(latex_options = "hold_position") %>%
    writeLines("output/tables/share_table.tex")

# Compute standardized bias and difference in means with p-value

# slightly adapted from lab session 2
balance_check.model <- function(x, treat){
  
  # Conditional means
  mean_d0 <- mean(x[treat==0])
  mean_d1 <- mean(x[treat==1])
  
  # Variances in subsamples
  var_d0 <- var(x[treat==0])
  var_d1 <- var(x[treat==1])
  
  # Difference in means
  diff_d <- lm(x ~ treat)
  cov <- vcovHC(diff_d, type = "HC")
  robust.se <- sqrt(diag(cov))
  
  # Absolute standardized bias (difference in means over average std dev - assuming similar size of the groups)
  sb <- abs((mean_d1-mean_d0)/sqrt((var_d0+var_d1)/2))*100
  
  # Store output as a list 
  list(mean_d0 = mean_d0, 
       mean_d1 = mean_d1,
       diff_d = diff_d$coefficients[2], 
       robust.se = robust.se[2], 
       pval = 2*pnorm(-abs(diff_d$coefficients[2]/robust.se[2])),
       SB = sb)             
}

balance_check <- balance_check.model(raw.data$below40, raw.data$treat)

# Prepare tex table with standardized bias and difference in means
balance_table <- tibble(
    "Variable" = "Age",
    "Standardized Bias" = balance_check$SB,
    "Difference in Means" = balance_check$diff_d,
    "P-value" = balance_check$pval
)

# Export table
balance_table %>%
    kableExtra::kable("latex", booktabs = TRUE, digits = 3, caption = "Balance Metrics") %>%
    kableExtra::kable_styling(latex_options = "hold_position") %>%
    writeLines("output/tables/balance_table.tex")

# --------------------------------------------------------------
# Question 2: IPW ATE estimation by age group
# --------------------------------------------------------------

# # issue: some individuals appear multiple times in the dataset
# # solution: use all unemployment spells until they are first treated
# subset.data <- raw.data %>%
#   group_by(id) %>%
#   mutate(
#     id_ = row_number(),
#     treat_first = ifelse(length(which(treat==1)) > 0, min(which(treat==1)), length(treat))
#     ) %>%
#   filter(id_ <= treat_first) %>%
#   ungroup()

# (a) Create 24 dummy variables for employment in first 24 months

# Create M dummies that are TRUE if date_end - date_start is larger than m months
for (m in 1:24) {
  raw.data[,paste0("emp_", m)] <- ifelse(lubridate::as.period(raw.data$date_end - raw.data$date_start) / months(1) < m, 1, 0)
}


# (b) Estimate propensity scores for <40 and ≥40 samples

# Prepare the formula for the propensity score model
pscore.formula <- as.formula(paste("treat ~", paste(covs, collapse = " + ")))

# Estimate the treatment probability parametrically using a probit model
pscore.model <- list(
    below40 = glm(pscore.formula, data = raw.data[raw.data$below40 == 1, ], family = binomial(link = "probit")),
    above40 = glm(pscore.formula, data = raw.data[raw.data$below40 == 0, ], family = binomial(link = "probit"))
)

# Store the propensity scores in the dataset
raw.data[raw.data$below40 == 1, "pscore_below40"] <- pscore.model$below40$fitted.values
raw.data[raw.data$below40 == 0, "pscore_above40"] <- pscore.model$above40$fitted.values

# # Check for common support
# ggplot(raw.data, aes(x = pscore, fill = treat_f)) +
#     geom_density(alpha=0.4) +
#     scale_fill_grey()+
#     theme_bw(base_size = 20) +
#     xlim(0, 1) +
#     facet_wrap(~below40_f)

# We drop observations with propensity scores of 0 or 1 as well as those outside the common support
raw.data <- raw.data %>%
    mutate(
        insample_below40 = 
            ((pscore_below40 < max(pscore_below40[treat == 0], na.rm = T)) & 
             (pscore_below40 > min(pscore_below40[treat == 1], na.rm = T)) & 
             !(pscore_below40 %in% c(0, 1))) * 1,
        insample_above40 = 
            ((pscore_above40 < max(pscore_above40[treat == 0], na.rm = T)) & 
             (pscore_above40 > min(pscore_above40[treat == 1], na.rm = T)) & 
             !(pscore_above40 %in% c(0, 1))) * 1
    )

sum(!raw.data$insample_below40, na.rm = T) # 4 observations are dropped
sum(!raw.data$insample_above40, na.rm = T) # 4 observations are dropped

trimmed.data <- list(
    below40 = raw.data %>%
        filter(insample_below40 == 1),
    above40 = raw.data %>%
        filter(insample_above40 == 1)
)

trimmed.data[["above40"]]

# (c) Estimate ATE using IPW for both groups

# estimate ATE for both age groups
ipw_atet <- list(below40 = list(), above40 = list())
for (age in c("below40", "above40")) {
    for (m in 1:24) {
        ipw_atet[[age]][[m]] <- treatweight(
            y = trimmed.data[[age]][[paste0("emp_", m)]],
            d = trimmed.data[[age]]$treat,
            x = trimmed.data[[age]][covs],
            trim = 0,
            boot = 100
        )
    }
}
# store results
save(ipw_atet, file = "code/Assignment_1_ipw_atet.RData")

# load("1/ipw_atet.RData")

# combine results into a single data frame
ipw_atet_table <-
    bind_rows(
        bind_cols(
            data.table::rbindlist(ipw_atet[["above40"]]),
            age = "above40",
            m = 1:24
        ),
        bind_cols(
            data.table::rbindlist(ipw_atet[["below40"]]),
            age = "below40",
            m = 1:24
        )
    )

# export booktabs table
ipw_atet_table %>%
    select("age group" = age, "time relative to treatment" = m, effect, se) %>%
    kableExtra::kable("latex", booktabs = TRUE, digits = 3, caption = "Program and Employment: Semiparametric ATE with IPW") %>%
    kableExtra::kable_styling(latex_options = "hold_position") %>%
    writeLines("output/tables/ipw_atet_table.tex")

# --------------------------------------------------------------
# Question 3: Plot ATEs and Interpretation
# --------------------------------------------------------------

# compute 95% confidence intervals
ipw_atet_table <- ipw_atet_table %>%
    mutate(
        lower = effect - 1.96 * se,
        upper = effect + 1.96 * se
    )

# Plot ATEs over 24 months with 95% CI for each age group
ggplot(ipw_atet_table, aes(x = m, y = effect, color = age)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    theme_bw(base_size = 20) +
    xlab("Months") +
    ylab("ATE") +
    ggtitle("ATEs over 24 months by age group") +
    scale_color_manual(values = c("blue", "red"))

ggsave("output/figures/ATEs_over_24_months_by_age_group.png")

# --------------------------------------------------------------
# End of script
# --------------------------------------------------------------