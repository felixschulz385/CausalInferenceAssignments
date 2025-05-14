# Lab Assignment 3 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 02.05.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())
rstudioapi::getActiveDocumentContext()$path %>% dirname() %>% dirname() %>% setwd()
load("code/AngristEvans1980_reduced.RData")

packages.vector <- c("dplyr", "stargazer", "sandwich", "lmtest", "AER", "broom", "broom.mixed", "jtools", "texreg", "kableExtra")
# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)

# Turn into tibble
data <- tibble(data)


# --------------------------------------------------------------
# Question 1
# --------------------------------------------------------------

# a)

data_married <- data[data$msample == 1, ]

avg_age_mom <- mean(data_married$agefstm, na.rm = TRUE)
avg_age_dad <- mean(data_married$agefstd, na.rm = TRUE)
# NOTE: this is not the income when the first child was born
avg_inc_mom <- mean(data_married$incomem, na.rm = TRUE)
avg_inc_dad <- mean(data_married$incomed, na.rm = TRUE)

stats <- tibble(
  "Age at First Birth" = c(avg_age_mom, avg_age_dad),
  "Average Income" = c(avg_inc_mom, avg_inc_dad)
) %>%
  mutate(Parent = c("Mother", "Father"), .before = 1)

stats %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Summary Statistics of Parents",
    label = "tab:parents_stats",
    digits = 2,
    escape = FALSE,
    align = "c",
  ) %>%
  writeLines("output/tables/3_parents_stats.tex")


# b)

# First stage regression w/o controls
first_stage1b <- lm(morekids ~ samesex, data = data_married)
robust.se.first.stage1b <- sqrt(diag(vcovHC(first_stage1b, type = "HC")))
robust.p.val.first.stage1b <- 2 * (1 - pnorm(abs(coef(first_stage1b) / robust.se.first.stage1b)))

iv_model1b <- ivreg(hourswd ~ morekids | samesex, data = data_married)
robust.se.iv_model1b <- sqrt(diag(vcovHC(iv_model1b, type = "HC")))
robust.p.val.iv_model1b <- 2 * (1 - pnorm(abs(coef(iv_model1b) / robust.se.iv_model1b)))

# c)

# First stage regression
first_stage1c <- lm(morekids ~ boys2 + girls2, data = data_married)
robust.se.first.stage1c <- sqrt(diag(vcovHC(first_stage1c, type = "HC")))
robust.p.val.first.stage1c <- 2 * (1 - pnorm(abs(coef(first_stage1c) / robust.se.first.stage1c)))

# Second stage regression w/o controls
iv_model1c <- ivreg(hourswd ~ morekids | boys2 + girls2, data = data_married)
robust.se.iv_model1c <- sqrt(diag(vcovHC(iv_model1c, type = "HC")))
robust.p.val.iv_model1c <- 2 * (1 - pnorm(abs(coef(iv_model1c) / robust.se.iv_model1c)))

# e)

ols_model1e <- lm(hourswd ~ morekids, data = data_married)
robust.se.ols_model1e <- sqrt(diag(vcovHC(ols_model1e, type = "HC")))
robust.p.val.ols_model1e <- 2 * (1 - pnorm(abs(coef(ols_model1e) / robust.se.ols_model1e)))

texreg(
  list(first_stage1b, first_stage1c, iv_model1b, iv_model1c, ols_model1e),
  file = "output/tables/3_iv_ols_hourswd.tex",
  #custom.coef.names = c("samesex"),
  custom.model.names = c("1(b)1", "1(c)1", "1(b)2", "1(c)2", "1(e)"),
  custom.header = list(
    "First Stage" = 1:2,
    "Second Stage" = 3:4,
    "OLS" = 5
  ),
  stars = c(0.001, 0.01, 0.05),
  override.se = list(robust.se.first.stage1b, robust.se.first.stage1c, robust.se.iv_model1b, robust.se.iv_model1c, robust.se.ols_model1e),
  override.pvalues = list(robust.p.val.first.stage1b, robust.p.val.first.stage1c, robust.p.val.iv_model1b, robust.p.val.iv_model1c, robust.p.val.ols_model1e),
  dcolumn = TRUE,
  float.pos = "H",
  booktabs = TRUE,
  use.packages = FALSE,
  table = FALSE,
  label = "tab:iv_ols_hourswd",
  caption = "Dad's hours worked and fertility: OLS and IV",
)

# --------------------------------------------------------------
# Question 2
# --------------------------------------------------------------

# a) median age mother and share of women with >2 children

median_age <- median(data_married$agem, na.rm = TRUE)
median_age %>% paste0(collapse = "") %>% writeLines("output/other/3_median_age.tex")

data_married$age_group <- ifelse(
  data_married$agem <= median_age,
  paste0("$age \\leq ", median_age, "$"),
  paste0("$age > ", median_age, "$")
  ) %>% as.factor()
                        
data_married %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    morekids_share = mean(morekids == 1, na.rm = TRUE),
    n = n()
  ) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Share with \\textgreater 2 Children by Age Group",
    label = "tab:children_age_group",
    digits = 2,
    col.names = c("Age Group", "Share with \\textgreater 2 Children", "N"),
    escape = FALSE,
    align = "c",
  ) %>%
  writeLines("output/tables/3_children_age_group.tex")

# b) OLS estimation on weeksm

data_married_below <- data_married %>%
  filter(agem <= median_age)

ols_model2b <- lm(weeksm ~ morekids + agem + agefstm + blackm + hispm + othracem, data = data_married_below)
robust.se.ols_model2b <- sqrt(diag(vcovHC(ols_model2b, type = "HC")))
robust.p.val.ols_model2b <- 2 * (1 - pnorm(abs(coef(ols_model2b) / robust.se.ols_model2b)))

# c) 2SLS estimate

# Second stage regression with controls
iv_model2c <- ivreg(weeksm ~ morekids + agem + agefstm + blackm + hispm + othracem | samesex + agem + agefstm + blackm + hispm + othracem , data = data_married_below)
robust.se.iv_model2c <- sqrt(diag(vcovHC(iv_model2c, type = "HC")))
robust.p.val.iv_model2c <- 2 * (1 - pnorm(abs(coef(iv_model2c) / robust.se.iv_model2c)))

# output table
texreg(
  list(ols_model2b, iv_model2c),
  file = "output/tables/3_iv_ols_weeksm.tex",
  custom.model.names = c("2(b)", "2(c)"),
  stars = c(0.001, 0.01, 0.05),
  override.se = list(robust.se.ols_model2b, robust.se.iv_model2c),
  override.pvalues = list(robust.p.val.ols_model2b, robust.p.val.iv_model2c),
  dcolumn = TRUE,
  float.pos = "H",
  booktabs = TRUE,
  use.packages = FALSE,
  label = "tab:ols_iv_weeksm",
  table = FALSE,
  caption = "Mother's weeks worked and fertility: OLS and IV",
)

# --------------------------------------------------------------
# Question 3
# --------------------------------------------------------------

# a)

# Calculate statistics and format into text
stats <- data_married %>%
  reframe(
    participm = mean(hourswm > 0),
    participd = mean(hourswd > 0),
    parttimem = 1 - mean(hourswm[hourswm > 0] >= 40),
    parttimed = 1 - mean(hourswd[hourswd > 0] >= 40),
  )

# Format text
participation_text <- sprintf(
  "The female labor participation rate is %.1f\\%% while the male labor participation rate is %.1f\\%%. The share of parttime workers is %.1f\\%% for moms and %.1f\\%% for dads.",
  stats$participm * 100,
  stats$participd * 100,
  stats$parttimem * 100,
  stats$parttimed * 100
)

# Write to file
writeLines(participation_text, "output/other/3_participation.tex")