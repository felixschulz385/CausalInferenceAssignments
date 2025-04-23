# Lab Assignment 3 – Causal Inference for Policy Evaluation
# Spring Semester 2025
# Due: 02.05.2025, 23:59
# Authors: Marco Gortan, Felix Schulz, Benjamin Weggelaar

# --------------------------------------------------------------
# Setup
# --------------------------------------------------------------

# Load the data
rm(list = ls())
load("code/AngristEvans1980_reduced.RData")

# Load necessary libraries
lapply(packages.vector, require, character.only = TRUE)

# Turn into tibble
raw.data <- tibble(raw.data)


# --------------------------------------------------------------
# Question 1
# --------------------------------------------------------------


# a)

data_married <- data[data$msample == 1, ]


avg_age_mom <- mean(data_married$agefstm, na.rm = TRUE)
avg_age_dad <- mean(data_married$agefstd, na.rm = TRUE)
avg_inc_mom <- mean(data_married$incomem, na.rm = TRUE)
avg_inc_dad <- mean(data_married$incomed, na.rm = TRUE)

# Your summary stats
stats <- c(avg_age_mom, avg_age_dad, avg_inc_mom, avg_inc_dad)
names(stats) <- c("Average age (Mother)", "Average age (Father)",
                  "Average income (Mother)", "Average income (Father)")

# Stargazer table
stargazer(stats, type = "latex", summary = FALSE, title = "Parental Characteristics at First Birth")



# b)

# First stage regression w/o controls
first_stage1 <- lm(morekids ~ samesex, data = data_married)
cov <- vcovHC(first_stage1, type = "HC")
robust.se.first.stage1 <- sqrt(diag(cov))
summ(first_stage1, robust = "HC1")

# Second stage regression w/o controls
iv_model1 <- ivreg(hourswd ~ morekids | samesex, data = data_married)
summary(iv_model1, vcov = sandwich, diagnostics = TRUE)

# c)

# First stage regression
first_stage2 <- lm(morekids ~ boys2 + girls2, data = data_married)
cov <- vcovHC(first_stage2, type = "HC")
robust.se.first.stage2 <- sqrt(diag(cov))
summ(first_stage2, robust = "HC1")

# Second stage regression w/o controls
iv_model2 <- ivreg(hourswd ~ morekids | boys2 + girls2, data = data_married)
summary(iv_model2, vcov = sandwich, diagnostics = TRUE)

# c)

#Weirdly enough, all estimates are not significant?? is this what the exercise asks sus to do?

# d) OLS estimate

ols_model1 <- lm(hourswd ~ morekids, data = data_married)
summ(ols_model1, robust = "HC1")


# --------------------------------------------------------------
# Question 2
# --------------------------------------------------------------


# a) median age mother and share of women with >2 children

median_age <- median(data_married$agem, na.rm = TRUE)
sum(is.na(data_married$agem))

data_married$age_group <- ifelse(data_married$agem <= median_age, "below", "above")
                        
data_married %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    share_more_than_2kids = mean(morekids == 1, na.rm = TRUE),
    n = n()
  )

# b) OLS estimation on Weeksm

data_married_below <- data_married %>%
        filter(agem < median_age)


ols_model2 <- lm(weeksm ~ morekids + agem + agefstm + blackm + hispm + othracem, data = data_married_below)
summary(ols_model2)


# c) 2SLS estimate 

# First stage regression
first_stage3 <- lm(morekids ~ samesex, data = data_married_below)
cov <- vcovHC(first_stage3, type = "HC")
robust.se.first.stage3 <- sqrt(diag(cov))
summ(first_stage3, robust = "HC1")

# Second stage regression with controls
iv_model3 <- ivreg(weeksm ~ morekids + agem + agefstm + blackm + hispm + othracem | samesex, data = data_married_below)
summary(iv_model3, vcov = sandwich, diagnostics = TRUE)


