# Lab Final Assignment – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 15.07.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------
Sys.setlocale("LC_TIME", "C")  
packages.vector <- c("dplyr", "stargazer", "sandwich", "lmtest", "AER", "broom", "broom.mixed", 
                     "jtools", "texreg", "kableExtra", "rstudioapi", "lubridate", "ggplot2", 
                     "knitr", "kableExtra", "scales", "tidyr", "causalweight", "ggpubr")
# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)
# Load the data
rm(list = ls())
rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()
data = read.csv("code/data_did_panel_swiss.csv")

# General reference for the column names. Note:
# - We have character values for marits, educ, mother_tongue, while the description has factors
# - "Specialized" and "University" are supposed to be comined in "educ" variable, but in our data they are different 
var_labels <- c(
  id              = "Personal identifier (the same individual can have multiple spells)",
  date_start      = "Date of start of unemployment spell",
  date_end        = "Date of end of unemployment spell",
  sex             = "Sex — 0 male, 1 female",
  age             = "Age at the beginning of the unemployment spell",
  marits          = "Marital status — 0 not married, 1 married",
  canton          = "Canton of residence — 26 cantons",
  region          = "Region of residence — 7 great regions (Swiss FSO definition)",
  nationality     = "Nationality — 1 Swiss, 2 EU/EFTA, 3 Other",
  mother_tongue   = "Mother tongue — 0 German, 1 French, 2 Italian, 3 English, 4 other",
  educ            = "Education — 0 school, 1 apprenticeship or matura, 2 specialized or university",
  insured_earn    = "Earnings insured by unemployment insurance",
  lastj_occpt     = "Occupation of the last pre‐unemployment job (23 categories)",
  lastj_fct       = "Function in the last pre‐unemployment job — 0 self‐employed, 1 managerial, 2 specialist, 3 learning/other",
  lastj_rate      = "Activity rate (% of full‐time) in the last pre‐unemployment job",
  child_subsidies = "1 receives subsidies for dependent children, 0 otherwise",
  contr_2y        = "Months of contribution (employment) in the 2 years prior to unemployment",
  unempl_r        = "Unemployment rate (in %) at canton level",
  gdp_gr          = "GDP growth (in %) at canton level",
  treat           = "Month in unemployment spell when treatment started (0 when not treated)"
)

# ----------
# Question 2
# ----------
# Generate the two outcome variables of interest and provide descriptive statistics for all four
# relevant groups (treated pre/post, controls pre/post). Discuss the results. [4 points]
# ----------
# Convert date_start and date_end to Date format
data$date_start         <- as.Date(data$date_start, format = "%d%b%Y")
data$date_end           <- as.Date(data$date_end, format = "%d%b%Y")
data$date_start_program <- ifelse(
  data$treat > 0,
  data$date_start %m+% months(data$treat-1),
  data$date_start)
data$date_start_program <- as.Date(data$date_start_program, origin = "1970-01-01")
# Remove individuals with 'NA' in 'treat' column 
data <- data %>% filter(!is.na(treat))
# treatment groups
data$treat_group <- ifelse(data$treat > 0, 1, 0)
data <- data %>%
  group_by(id) %>%
  mutate(treat_group = max(treat_group, na.rm = TRUE)) %>%
  ungroup()

data$post        <- ifelse(year(data$date_start) >= 2013, 1, 0)

# 1. unemployment duration
data$unempl_duration <- as.numeric(data$date_end - data$date_start, format = "%d%b%Y")
data$unempl_duration_fromprogram <- as.numeric(data$date_end - data$date_start_program, format = "%d%b%Y")

# 2. probability of being employed after 12 months --> for now, we interpret as just dummy of employed vs non-employed
data$employed_after_12_months <- ifelse(data$unempl_duration < 365, 1, 0)
data$employed_after_12_months_fromprogram <- ifelse(data$date_start_program < 365, 1, 0)

# Descriptive statistics for the four groups and the two outcomes
# ------
# Density plots
# 1) Make a combined group factor:
dataplot <- data %>%
  mutate(
    Treat = factor(treat_group,
                   levels = c(0, 1),
                   labels = c("Control", "Treated")),
    Period = factor(post,
                    levels = c(0, 1),
                    labels = c("Pre", "Post"))
  )

# 2) Density plot with color = Treat, linetype = Period
plot_unemdays <- ggplot(dataplot, aes(
  x = unempl_duration,
  color     = Treat,
  linetype = Period
)) +
  geom_density(size = 1) +
  scale_color_manual(
    name   = "Group",
    values = c("Control" = "#1b9e77", "Treated" = "#d95f02")
  ) +
  scale_linetype_manual(
    name   = "Period",
    values = c("Pre"  = "solid", "Post" = "dashed")
  ) +
  labs(
    x     = "Unemployment Duration (days)",
    y     = "Density",
    title = "Density of Unemployment Duration by Group & Period"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "top",
    legend.box        = "horizontal",
    panel.grid.minor  = element_blank()
  )

# Save the plot
ggsave("output/figures/final_unemployment_duration_density.jpg", plot = plot_unemdays, width = 6.5, height = 5)

# Employed after 12 months
bin_summary <- dataplot %>%
  group_by(Treat, Period) %>%
  summarise(
    prop = mean(employed_after_12_months, na.rm = TRUE),
    n    = n(),
    se   = sqrt(prop * (1 - prop) / n),
    .groups = "drop"
  )

# 1b) bar chart with CIs
plot_employed12m <- ggplot(bin_summary, aes(x = Treat, y = prop, fill = Period)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(
    ymin = pmax(0, prop - 1.96 * se),
    ymax = pmin(1, prop + 1.96 * se)
  ),
  position = position_dodge(width = 0.7),
  width = 0.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0,0.05))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    x = "Group",
    y = "Employed after 12 months (%)",
    title = "Proportion Employed by Group and Period",
    fill = "Period"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("output/figures/final_employment12m.jpg", plot = plot_employed12m, width = 6.5, height = 5)




# ----------
# Question 3
# ----------
# Compare mean observed characteristics for all four relevant groups (treated pre/post, controls
# pre/post). Discuss the results. [6 points]
# ----------


# Codify sex, marital status, variable
data$sex <- ifelse(data$sex == "Female", 1,
                     ifelse(data$sex == "Male", 0, NA))
data$marits <- ifelse(data$marits == "Married", 1,
                   ifelse(data$marits == "Unmarried", 0, NA))
# Descriptive statistics for the four groups
descriptive_stats <- data %>%
  group_by(treat_group, post) %>%
  summarise(
    n = n(),
    mean_age      = mean(age, na.rm = T),
    mean_sex      = mean(sex, na.rm = TRUE),
    mean_marits   = mean(marits, na.rm = TRUE),
    mean_earn     = mean(insured_earn, na.rm = TRUE),
    mean_rate     = mean(lastj_rate, na.rm = TRUE),
    mean_childs   = mean(child_subsidies, na.rm = TRUE),
    mean_contr_2y = mean(contr_2y, na.rm = TRUE),
    .groups = 'drop'
  )

descriptive_tbl <- descriptive_stats %>%
  mutate(
    mean_sex           = mean_sex       * 100,
    mean_marits        = mean_marits    * 100,
    mean_childs        = mean_childs    * 100,
    mean_age           = mean_age,               # years
    mean_earn          = mean_earn,              # currency
    mean_rate          = mean_rate,              # %
    mean_contr_2y      = mean_contr_2y           # months
  ) %>%
  # round to one decimal place
  mutate(across(starts_with("mean_"), ~round(.x, 1))) %>%
  # rename for nicer column headers
  rename(
    Treatment            = treat_group,
    `Post`    = post,
    N                = n,
    `Age (yrs)`      = mean_age,
    `Female (%)`     = mean_sex,
    `Married (%)`    = mean_marits,
    `Earnings`       = mean_earn,
    `Act. rate (%)`  = mean_rate,
    `Child sub. (%)` = mean_childs,
    `Contr. 2y (m)`  = mean_contr_2y
  )

# 2) Print to LaTeX with group headers
desc_stat = descriptive_tbl %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Mean observed characheristics by Treatment Group and Period",
    label   = "tab:final_mean_char",
  ) %>%
  add_header_above(c(
    " " = 2,
    "Demographics" = 3,
    "Economic"     = 2,
    "Other"        = 3
  )) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write to file
writeLines(desc_stat, "output/tables/final_mean_char.tex")

# ----------
# Question 5
# ----------
# Plot average outcomes for treatment and control group for each month in the observation
# period. Discuss the results. [6 points]
# ----------
for (m in 1:24) {
  data[,paste0("empdummy_", m)] <- ifelse(lubridate::as.period(data$date_end - data$date_start) / months(1) < m, 1, 0)
}

for (m in 1:24) {
  data[,paste0("empdummyprogram_", m)] <- ifelse(lubridate::as.period(data$date_end - data$date_start_program) / months(1) < m, 1, 0)
}

# Create column with number of days since beginning of the month
data$days_since_month_start <- as.numeric(data$date_start - floor_date(data$date_start, "month"))
for (m in 1:24) {
  data[,paste0("unempduration_", m)] <- ifelse(lubridate::as.period(data$date_end - data$date_start) / months(1) < m, 
                                             data$unempl_duration, 
                                             m*30.4-data$days_since_month_start # this is rough estimate of the number of days in a month
                                             )
}

# Create column with number of days since beginning of program (does not change anything for non-treated)
data$days_since_month_start_program <- as.numeric(data$date_start_program - floor_date(data$date_start_program, "month"))
for (m in 1:24) {
  data[,paste0("unempdurationprogram_", m)] <- ifelse(lubridate::as.period(data$date_end - data$date_start_program) / months(1) < m, 
                                               data$unempl_duration_fromprogram, 
                                               m*30.4-data$days_since_month_start_program # this is rough estimate of the number of days in a month
  )
}

# Create a long format data frame for plotting
data_long_obs <- data %>%
  filter(post == 1) %>%
  # keep only the id/group vars plus any empdummy_*/unempduration_* columns
  select(id, treat_group, matches("^(empdummy|empdummyprogram|unempduration|unempdurationprogram)_")) %>%
  # pivot both sets of columns in one go:
  pivot_longer(
    cols = matches("^(empdummy|empdummyprogram|unempduration|unempdurationprogram)_"),
    names_to = c(".value", "month"),
    names_sep = "_"
  ) %>%
  # rename for clarity
  rename(
    employed        = empdummy,
    employed_fromprogram = empdummyprogram,
    unempl_duration = unempduration,
    unempl_duration_fromprogram = unempdurationprogram
  ) %>%
  # make month numeric
  mutate(month = as.integer(month))

# Calculate means for each month and group
data_long_means <- data_long_obs %>%
  group_by(treat_group, month) %>%
  summarise(
    mean_employed        = mean(employed, na.rm = TRUE),
    mean_employed_program = mean(employed_fromprogram, na.rm = TRUE),
    mean_unempl_duration = mean(unempl_duration, na.rm = TRUE),
    mean_unempl_duration_program = mean(unempl_duration_fromprogram, na.rm = TRUE),
    .groups = 'drop'
  )

# Create the plot
data_long_means <- data_long_means %>%   # replace with your actual data‐frame name
  mutate(
    Group = factor(treat_group,
                   levels = c(0, 1),
                   labels = c("Control", "Treated"))
  )

# ————————————————————————————————————————————
# Mean employment rate over time
# ————————————————————————————————————————————
p1 <- ggplot(data_long_means, aes(x = month, y = mean_employed, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # ==== new layer: treated-only line ====
geom_line(
  data = filter(data_long_means, treat_group == 1),
  aes(x = month, y = mean_employed_program),
  color = "#d95f02",
  linetype = "dashed",
  size = 1
) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, .05))
  ) +
  scale_x_continuous(
    breaks = seq(min(data_long_means$month), max(data_long_means$month), by = 1)
  ) +
  scale_color_manual(
    values = c("Control" = "#1b9e77", "Treated" = "#d95f02")
  ) +
  labs(
    title = "Employment Rate since beginning of unemployment",
    x     = "Month",
    y     = "Employment rate (%)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "top",
    panel.grid.minor  = element_blank()
  )

# Print it
print(p1)


# ————————————————————————————————————————————
# Mean unemployment duration over time
# ————————————————————————————————————————————
p2 <- ggplot(data_long_means, aes(x = month, y = mean_unempl_duration, color = Group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05))
  ) +
  # ==== new layer: treated-only line ====
  geom_line(
    data = filter(data_long_means, treat_group == 1),
    aes(x = month, y = mean_unempl_duration_program),
    color = "#d95f02",
    linetype = "dashed",
    size = 1
  ) +
  scale_x_continuous(breaks = seq(min(data_long_means$month), max(data_long_means$month), by = 1)) +
  scale_color_manual(
    values = c("Control" = "#1b9e77", "Treated" = "#d95f02")
  ) +
  labs(
    title = "Unemployment Duration",
    x     = "Month",
    y     = "Mean Unemployment Duration (days)",
    color = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "top",
    panel.grid.minor  = element_blank()
  )
print(p2)

# Save the plots
ggsave("output/figures/final_employment_rate_over_time.jpg", plot = p1, width = 6.5, height = 5)
ggsave("output/figures/final_unemployment_duration_over_time.jpg", plot = p2, width = 6.5, height = 5)


# ----------
# Question 6
# ----------
# Discuss the validity of the identifying assumptions in this specific case. Provide and discuss
# supporting evidence if possible, incl. event study estimates. [8 points]

# Parallel trends assumption
# ----------

data_post <- data %>%
  filter(post == 1) %>%
  mutate(
    tmonth_start = date_start %>% `day<-`(1),
    tmonth_end   = date_end %>% `day<-`(1),
    mtime_start = ceiling((ymd(min(date_start, na.rm = T)) %--% tmonth_start) / dmonths(1)) + 1,
    mtime_end = ceiling((ymd(min(date_start, na.rm = T)) %--% tmonth_end) / dmonths(1)) + 1
  ) %>%
  select(id, mtime_start, mtime_end, treat) %>%
  drop_na()

create_subpanel <- function(id, mtime_start, mtime_end, treat) {
  tibble(
    id              = id,
    mtime           = seq(mtime_start, 24, 1),
    gtime           = ifelse(treat > 1, mtime_start + (treat - 1), 0),
    employed        = ifelse(((mtime >= mtime_start) & (mtime < mtime_end)), 0, 1),
  )
}

event_study_panel <- purrr::pmap_df(data_post, create_subpanel)

library(did)

att_gt(
  yname     = "employed",
  tname     = "mtime",
  gname     = "gtime",
  idname    = "id",
  data      = event_study_panel,
  control_group = "nevertreated"
) -> est

aggte(est, type = "dynamic", na.rm = T) -> es

ggdid(
  es,
  xlab = "Months since treatment",
  ylab = "Employment rate",
  title = "Event Study: Employment Rate by Months since Treatment",
  theme = theme_minimal(base_size = 14)
)
ggsave("output/figures/final_event_study_employment_rate.jpg", width = 6.5, height = 5)

# Common support
#----------
# Hist plot of age by the two groups in post == 1
data <- data %>%
  mutate(
    Group = factor(treat_group,
                   levels = c(0,1),
                   labels = c("Control","Treated"))
  )

hist_age = ggplot(data %>% filter(post == 1), aes(x = age + 1, fill = Group)) +
  geom_histogram(
    position = "stack",  # overlay
    alpha    = 0.4,         # see through
    bins     = 30,           # or choose binwidth = 1
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Age Distribution by Group",
    x     = "Age (years)",
    y     = "Count"
  ) +
  theme_minimal()

ggsave("output/figures/final_histogram_age.jpg", plot = hist_age, width = 8.5, height = 5)


# ----------
# Question 7
# ----------
# Implement the DiD using OLS. Describe in detail what you do and why. Discuss whether or
# not you need additional control variables and if so, why. Discuss the results. [8 points]

# OLS DiD for unemployment duration
ols_results <- lm(unempl_duration ~ treat_group * post, data)
ols_vcov_clustered <- vcovCL(ols_results, cluster = ~ id)
ols_se <- sqrt(diag(ols_vcov_clustered))
ols_t <- coef(ols_results) / ols_se
ols_p <- 2 * pt(-abs(ols_t), df = df.residual(ols_results))

ols_covariates_results <- lm(
  unempl_duration ~ treat_group * post + 
    age + sex + marits + insured_earn + lastj_rate +
    child_subsidies + contr_2y,
  data = data
)
ols_covariates_vcov_clustered <- vcovCL(ols_covariates_results, cluster = ~ id)
ols_covariates_se <- sqrt(diag(ols_covariates_vcov_clustered))
ols_covariates_t <- coef(ols_covariates_results) / ols_covariates_se
ols_covariates_p <- 2 * pt(-abs(ols_covariates_t), df = df.residual(ols_covariates_results))

# OLS DiD for employment after 12 months
ols_results_emp <- lm(employed_after_12_months ~ treat_group * post, data)
ols_vcov_clustered_emp <- vcovCL(ols_results_emp, cluster = ~ id)
ols_se_emp <- sqrt(diag(ols_vcov_clustered_emp))
ols_t_emp <- coef(ols_results_emp) / ols_se_emp
ols_p_emp <- 2 * pt(-abs(ols_t_emp), df = df.residual(ols_results_emp))

ols_covariates_results_emp <- lm(
  employed_after_12_months ~ treat_group * post + 
    age + sex + marits + insured_earn + lastj_rate +
    child_subsidies + contr_2y,
  data = data
)
ols_covariates_vcov_clustered_emp <- vcovCL(ols_covariates_results_emp, cluster = ~ id)
ols_covariates_se_emp <- sqrt(diag(ols_covariates_vcov_clustered_emp))
ols_covariates_t_emp <- coef(ols_covariates_results_emp) / ols_covariates_se_emp
ols_covariates_p_emp <- 2 * pt(-abs(ols_covariates_t_emp), df = df.residual(ols_covariates_results_emp))


# Output the results for both outcomes in one table
texreg(
  list(
    ols_results, ols_covariates_results,
    ols_results_emp, ols_covariates_results_emp
  ),
  custom.header = list(
    "Unemployment Duration" = 1:2,
    "Employment After 12 Months" = 3:4
  ),
  custom.model.names = c(
    "(1a)", "(1b)", "(2a)", "(2b)"
  ),
  custom.coef.names = c(
    "(Intercept)", "Treated", "Post", "Treated * Post", "Age", "Sex", "Marital Status", 
    "Insured Earnings", "Last Job Rate", "Child Subsidies", "Contributions 2y"
  ),
  omit.coef = "(age)|(sex)|(marits)|(insured_earn)|(lastj_rate)|(child_subsidies)|(contr_2y)",
  stars = c(0.01, 0.05, 0.1),
  caption = "OLS Results for Unemployment Duration and Employment After 12 Months",
  label = "tab:final_ols_results_combined",
  file = "output/tables/final_ols_results_combined.tex",
  dcolumn = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,
  override.se = list(ols_se, ols_covariates_se, ols_se_emp, ols_covariates_se_emp),
  override.pvalues = list(ols_p, ols_covariates_p, ols_p_emp, ols_covariates_p_emp),
  custom.note = "Covariates include age, gender, marital status, earning insured by unemployment insurance, activity rate in the last job, receiving child subsidies, months of employment in the 2 years prior to unemployment. Standard errors clustered at the individual level. $^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$",
)

# ----------

# ----------
# Question 8
# ----------
# Implement the DiD using a semi-parametric estimator based on the propensity score. Describe
# in detail what you do and why. Discuss the results and compare them to the OLS estimates.
# [10 points]

# Implementation of the propensity score-based DiD estimator from slides
# We need to estimate three separate propensity score models for the three comparisons:
# 1. P(T=1, D=1 | T=1, D=1 vs T=1, D=0) - treated post vs treated pre
# 2. P(T=1, D=1 | T=1, D=1 vs T=0, D=1) - treated post vs controls post  
# 3. P(T=1, D=1 | T=1, D=1 vs T=0, D=0) - treated post vs controls pre

# Prepare data subsets for each comparison
data_clean <- data %>% filter(!is.na(unempl_duration) & !is.na(employed_after_12_months))

# Model 1: Treated post vs treated pre
data_comp1 <- data_clean %>% filter(treat_group == 1)
data_comp1$outcome_indicator <- ifelse(data_comp1$post == 1, 1, 0)

ps_model1 <- glm(
  outcome_indicator ~ age + sex + marits + insured_earn + lastj_rate + child_subsidies + contr_2y,
  data = data_comp1,
  family = binomial(link = "logit")
)
data_comp1$ps1 <- predict(ps_model1, type = "response")

# Model 2: Treated post vs controls post
data_comp2 <- data_clean %>% filter(post == 1)
data_comp2$outcome_indicator <- ifelse(data_comp2$treat_group == 1, 1, 0)

ps_model2 <- glm(
  outcome_indicator ~ age + sex + marits + insured_earn + lastj_rate + child_subsidies + contr_2y,
  data = data_comp2,
  family = binomial(link = "logit")
)
data_comp2$ps2 <- predict(ps_model2, type = "response")

# Model 3: Treated post vs controls pre
data_comp3 <- data_clean %>% filter((treat_group == 1 & post == 1) | (treat_group == 0 & post == 0))
data_comp3$outcome_indicator <- ifelse(data_comp3$treat_group == 1 & data_comp3$post == 1, 1, 0)

ps_model3 <- glm(
  outcome_indicator ~ age + sex + marits + insured_earn + lastj_rate + child_subsidies + contr_2y,
  data = data_comp3,
  family = binomial(link = "logit")
)
data_comp3$ps3 <- predict(ps_model3, type = "response")

# Function to calculate ATET using the slide formula
calculate_atet <- function(outcome_var) {
  
  # Extract relevant data
  treated_post <- data_clean %>% filter(treat_group == 1, post == 1)
  treated_pre <- data_clean %>% filter(treat_group == 1, post == 0)
  control_post <- data_clean %>% filter(treat_group == 0, post == 1)
  control_pre <- data_clean %>% filter(treat_group == 0, post == 0)
  
  # Merge propensity scores
  treated_pre <- treated_pre %>%
    left_join(data_comp1 %>% select(id, date_start, ps1), by = c("id", "date_start"))
  control_post <- control_post %>%
    left_join(data_comp2 %>% select(id, date_start, ps2), by = c("id", "date_start"))
  control_pre <- control_pre %>%
    left_join(data_comp3 %>% select(id, date_start, ps3), by = c("id", "date_start"))
  
  # Calculate weights w_{0,1}, w_{1,0}, w_{0,0} as shown in slides
  treated_pre$w01 <- (1 - treated_pre$ps1) / treated_pre$ps1
  control_post$w10 <- (1 - control_post$ps2) / control_post$ps2
  control_pre$w00 <- (1 - control_pre$ps3) / control_pre$ps3
  
  # Calculate weighted averages for each component
  # Term 1: E[Y | T=1, D=1]
  term1 <- mean(treated_post[[outcome_var]], na.rm = TRUE)
  
  # Term 2: E[Y(0,1) | T=1] = weighted average of treated pre
  if(sum(!is.na(treated_pre$w01)) > 0) {
    term2_num <- sum(treated_pre$w01 * treated_pre[[outcome_var]], na.rm = TRUE)
    term2_den <- sum(treated_pre$w01, na.rm = TRUE)
    term2 <- term2_num / term2_den
  } else {
    term2 <- mean(treated_pre[[outcome_var]], na.rm = TRUE)
  }
  
  # Term 3: E[Y(1,0) | T=1] = weighted average of controls post
  if(sum(!is.na(control_post$w10)) > 0) {
    term3_num <- sum(control_post$w10 * control_post[[outcome_var]], na.rm = TRUE)
    term3_den <- sum(control_post$w10, na.rm = TRUE)
    term3 <- term3_num / term3_den
  } else {
    term3 <- mean(control_post[[outcome_var]], na.rm = TRUE)
  }
  
  # Term 4: E[Y(0,0) | T=1] = weighted average of controls pre
  if(sum(!is.na(control_pre$w00)) > 0) {
    term4_num <- sum(control_pre$w00 * control_pre[[outcome_var]], na.rm = TRUE)
    term4_den <- sum(control_pre$w00, na.rm = TRUE)
    term4 <- term4_num / term4_den
  } else {
    term4 <- mean(control_pre[[outcome_var]], na.rm = TRUE)
  }
  
  # Calculate ATET = term1 - term2 - term3 + term4
  atet <- term1 - term2 - term3 + term4
  
  return(list(
    atet = atet,
    term1 = term1,
    term2 = term2, 
    term3 = term3,
    term4 = term4
  ))
}

# Calculate ATET for both outcomes
atet_duration <- calculate_atet("unempl_duration")
atet_employment <- calculate_atet("employed_after_12_months")

# Create a summary table for ATET results
comparison_results <- data.frame(
  Outcome = c("Unemployment Duration", "Employment after 12 months"),
  ATET = c(round(atet_duration$atet, 2), round(atet_employment$atet, 4)),
  `E[Y|T=1,D=1]` = c(round(atet_duration$term1, 2), round(atet_employment$term1, 4)),
  `E[Y(0,1)|T=1]` = c(round(atet_duration$term2, 2), round(atet_employment$term2, 4)),
  `E[Y(1,0)|T=1]` = c(round(atet_duration$term3, 2), round(atet_employment$term3, 4)),
  `E[Y(0,0)|T=1]` = c(round(atet_duration$term4, 2), round(atet_employment$term4, 4))
)

# Print as a nice table
kableExtra::kbl(
  comparison_results,
  format = "latex",
  col.names = c("Outcome", "ATET", "E[Y|T=1,D=1]", "E[Y(0,1)|T=1]", "E[Y(1,0)|T=1]", "E[Y(0,0)|T=1]"),
  digits = 2,
  caption = "ATET Results using Propensity Score Method from Slides",
  booktabs = TRUE,
  align = "lcccccc"
) %>%
  writeLines("output/tables/final_atet_results.tex")

# 1 x 3 plot with relation between age and pscore taken from data_comp1, data_comp2,
# data_comp3
ggps1 = ggplot(data_comp1, aes(x = age, y = ps1)) +
  geom_point(alpha = 0.5) +
  labs(title = "Propensity Score for Treated Post vs Treated Pre",
       x = "Age", y = "Propensity Score") +
  theme_minimal(base_size = 14)

ggps2 = ggplot(data_comp2, aes(x = age, y = ps2)) +
  geom_point(alpha = 0.5) +
  labs(title = "Propensity Score for Treated Post vs Controls Post",
       x = "Age", y = "Propensity Score") +
  theme_minimal(base_size = 14)

ggps3 = ggplot(data_comp3, aes(x = age, y = ps3)) +
  geom_point(alpha = 0.5) +
  labs(title = "Propensity Score for Treated Post vs Controls Pre",
       x = "Age", y = "Propensity Score") +
  theme_minimal(base_size = 14)

# Combine the three plots into one
ggarrange(
  ggps1, ggps2, ggps3,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE, legend = "bottom"
) +
  labs(title = "Propensity Scores by Age for Different Comparisons") +
  theme_minimal(base_size = 14)

data %>% nrow()

data %>% group_by(id) %>% summarise(n = n()) %>% pull(n) %>% table()

data %>% group_by(id) %>% summarise(pre = mean(date_start < as.Date("2013-01-01"))) %>% pull(pre) %>% table()


data %>% select(date_start) %>% 
  mutate(year = year(date_start)) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  labs(title = "Number of Unemployment Spells by Year", x = "Year", y = "Count") +
  theme_minimal(base_size = 14)

