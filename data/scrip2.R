#############################
## Sleep and Blood Pressure #
## Amos Okutse              #
## Monica Colon-Vargas      #
#############################

library(dplyr)
library(gtsummary)
library(survey)
library(ggcorrplot)
library(kableExtra)

## Load Data
dt <- read.csv("/Users/aokutse/Desktop/PhD/Fall 2023/PHP2601 Linear Models/ProjectFinal/data/dt.csv")

## load the variable name file
vars <- read.csv("/Users/aokutse/Desktop/PhD/Fall 2023/PHP2601 Linear Models/ProjectFinal/data/vars.csv")

## some pre-processing
dt <- dt %>% select(-X)
dt <- dt %>%
  mutate_if(is.character, as.factor)

## edit the citizenship levels: Citizen, not citizen/other
## Marital status: single/never married, married or living with partner, divorced or separated, widowed 

## summarize the data by gender
tabsum <- dt %>%  
  dplyr::select(-seq_no,-strata,-psu,-weights) %>%
  tbl_summary(digits = list(everything() ~ c(2)),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = gender,
              label = list(bmi ~ "BMI",
                           hdl ~ "HDL",
                           total_chol ~ "TC",
                           hemoglobin ~ "Hemoglobin",
                           albumin ~ "Albumin",
                           creatinine ~ "Creatinine",
                           hypertension ~ "Hypertension",
                           diabetes ~ "Diabetes",
                           citizenship_status ~ "Citizenship",
                           educ_level ~ "Education",
                           children..5 ~ "Children > 5 yrs",
                           age_yr ~ "Age (yrs)",
                           marital_status ~ "Marital status",
                           cycle ~ "Survey cycle",
                           dbp ~ "DBP",
                           sbp ~ "SBP",
                           sleep ~ "Sleep",
                           race ~ "Race",
                           smoke ~ "Smoking status",
                           snort ~ "Snort",
                           alcohol ~ "Alcohol",
                           income_category ~ "Income"
                           )) %>%
  add_overall() %>% 
  modify_header(label ~ "**Variable**") %>%
  bold_labels() %>% 
  add_p()


# ## Systolic Blood Pressure
# ## Load variables selected by LASSO
# lasso.coef.min <- read.csv("../ProjectFinal/SleepBP/data/lasso_res_sbp_min.csv")
# lasso.coef.min$x
# names.lasso.min <- c("bmi", "hdl", "total_chol", "hemoglobin", "albumin", "AST",
#                      "hypertension", "creatinine", "diabetes", "citizenship_status",
#                      "educ_level", "children..5", "gender", "age_yr", "marital_status",
#                      "sleep", "race", "smoke", "snort", "alcohol", "income_category")
# dt %>% select(all_of(names.lasso.min))
# 
# 
# ## Fit lm
# ## With min
# modlasso.sbp.min <- lm(sbp ~. ,data = dt %>% select(all_of(names.lasso.min),sbp)
# )
# summary(modlasso.sbp.min)
# pred <- predict(modlasso.sbp.min, newdata = dt %>% select(all_of(names.lasso.min)))
# sum((pred-dt$sbp)^2)/nrow(dt)
# tbl_regression(modlasso.sbp.min, include = everything()) %>%
#   modify_column_unhide(column = std.error)
# 
# ## Dyastolic Blood Pressure
# ## Load variables selected by LASSO
# lasso.coef.min <- read.csv("C:/Users/monic/OneDrive/Desktop/SleepBP/data/lasso_res_dbp_min.csv")
# lasso.coef.min$x
# names.lasso.min <- c("bmi","total_chol", "hemoglobin", "albumin", "AST",
#                      "hypertension", "diabetes", "citizenship_status",
#                      "educ_level", "children..5", "gender", "age_yr",
#                      "marital_status", "cycle", "sleep", "race", "snort", 
#                      "alcohol")
# dt %>% select(all_of(names.lasso.min))
# 
# ## Fit lm
# ## With Min
# modlasso.dbp.min <- lm(dbp ~. ,data = dt %>% select(all_of(names.lasso.min),dbp))
# summary(modlasso.dbp.min)
# pred <- predict(modlasso.dbp.min, newdata = dt %>% select(all_of(names.lasso.min)))
# sum((pred-dt$dbp)^2)/nrow(dt)
# 
# 
# tab1 <- tbl_regression(modlasso.sbp.min, include = everything()) %>%
#   modify_column_unhide(column = std.error)
# 
# tab2 <- tbl_regression(modlasso.dbp.min, include = everything()) %>%
#   modify_column_unhide(column = std.error)
# 
# tbl_merge(tbls = list(tab1, tab2),tab_spanner = c("**Systolic BP**", "**Dystolic BP**"))
# 
# AIC(modlasso.dbp.min, modlasso.sbp.min)

