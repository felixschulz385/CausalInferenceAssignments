# Lab Final Assignment – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 15.07.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------
packages.vector <- c("dplyr", "stargazer", "sandwich", "lmtest", "AER", "broom", "broom.mixed", "jtools", "texreg", "kableExtra", "rstudioapi")
# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)
# Load the data
rm(list = ls())
# rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()
data = read.csv("code/data_did_panel_swiss.csv")

# General reference for the column names. Note:
# - We have character values for marits, Educ, Mother_tongue, while the description has factors
# - "Specialized" and "University" are supposed to be comined in "Educ" variable, but in our data they are different 
var_labels <- c(
  Id              = "Personal identifier (the same individual can have multiple spells)",
  Date_start      = "Date of start of unemployment spell",
  Date_end        = "Date of end of unemployment spell",
  Sex             = "Sex — 0 male, 1 female",
  Age             = "Age at the beginning of the unemployment spell",
  Marits          = "Marital status — 0 not married, 1 married",
  Canton          = "Canton of residence — 26 cantons",
  Region          = "Region of residence — 7 great regions (Swiss FSO definition)",
  Nationality     = "Nationality — 1 Swiss, 2 EU/EFTA, 3 Other",
  Mother_tongue   = "Mother tongue — 0 German, 1 French, 2 Italian, 3 English, 4 other",
  Educ            = "Education — 0 school, 1 apprenticeship or matura, 2 specialized or university",
  Insured_earn    = "Earnings insured by unemployment insurance",
  Lastj_occpt     = "Occupation of the last pre‐unemployment job (23 categories)",
  Lastj_fct       = "Function in the last pre‐unemployment job — 0 self‐employed, 1 managerial, 2 specialist, 3 learning/other",
  Lastj_rate      = "Activity rate (% of full‐time) in the last pre‐unemployment job",
  Child_subsidies = "1 receives subsidies for dependent children, 0 otherwise",
  Contr_2y        = "Months of contribution (employment) in the 2 years prior to unemployment",
  Unempl_r        = "Unemployment rate (in %) at canton level",
  GDP_gr          = "GDP growth (in %) at canton level",
  Treat           = "Month in unemployment spell when treatment started (0 when not treated)"
)