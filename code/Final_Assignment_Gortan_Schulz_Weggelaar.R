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
                     "knitr", "kableExtra")
# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)
# Load the data
rm(list = ls())
# rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()
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
data$date_start <- as.Date(data$date_start, format = "%d%b%Y")
data$date_end <- as.Date(data$date_end, format = "%d%b%Y")
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
# 2. probability of being employed after 12 months --> for now, we interpret as just dummy of employed vs non-employed
data$employed_after_12_months <- ifelse(data$unempl_duration < 365, 1, 0)
# Codify sex, marital status, variable
data$sex <- ifelse(data$sex == "Female", 1,
                     ifelse(data$sex == "Male", 0, NA))
data$marits <- ifelse(data$marits == "Married", 1,
                   ifelse(data$marits == "Unmarried", 0, NA))
# Descriptive statistics for the four groups
descriptive_stats <- data %>%
  group_by(treat_group, post) %>%
  summarise(
    mean_duration = mean(unempl_duration, na.rm = TRUE),
    mean_employed = mean(employed_after_12_months, na.rm = TRUE),
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
    mean_employed      = mean_employed  * 100,
    mean_sex           = mean_sex       * 100,
    mean_marits        = mean_marits    * 100,
    mean_childs        = mean_childs    * 100,
    mean_duration      = mean_duration,          # leave in days
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
    `Dur. (days)`    = mean_duration,
    `Empl. 1Y (%)`   = mean_employed,
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
    caption = "Descriptive Statistics by Treatment Group and Period",
    label   = "tab:desc_stats"
  ) %>%
  add_header_above(c(
    " " = 2,
    "Unemployment" = 2,
    "Demographics" = 3,
    "Economic"     = 2,
    "Other"        = 3
  )) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

# Write to file
writeLines(desc_stat, "output/tables/final_desc_stat.tex")
