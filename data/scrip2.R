#############################
## Sleep and Blood Pressure #
## Amos Okutse              #
## Monica Colon-Vargas      #
#############################

library(dplyr)
library(gtsummary)
library(survey)

## Load Data
dt <- read.csv("../ProjectFinal/data/dt.csv")
dt <- dt %>% select(-X)
dt <- dt %>%
  mutate_if(is.character,as.factor)


## Get Summary Statistics
dt %>%  dplyr::select(-seq_no) %>%
  tbl_summary(digits = list(everything() ~ c(2)),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              by = gender) %>%
  add_overall() %>% 
  modify_header(label ~ "**Sex**") %>%
  bold_labels()

## Systolic Blood Pressure
## Load variables selected by LASSO
lasso.coef.min <- read.csv("../SleepBP/data/lasso_res_sbp.csv")
lasso.coef.min$x
names.lasso.min <- c("bmi", "hdl", "total_chol", "hemoglobin", "albumin", "AST",
                     "hypertension", "creatinine", "diabetes", "citizenship_status",
                     "educ_level", "children..5", "gender", "age_yr", "marital_status",
                     "cycle", "sleep", "race", "smoke", "snort", "alcohol", 
                     "income_category")
dt %>% select(all_of(names.lasso.min))

lasso.coef.1se <- read.csv("../SleepBP/data/lasso_res_sbp_1se.csv")
lasso.coef.1se$x
names.lasso.1se <- c("total_chol", "hypertension", "educ_level", "gender",
                     "age_yr", "marital_status", "race", "income_category")
dt %>% select(all_of(names.lasso.1se),sbp)

## Fit lm
## With min
modlasso.sbp.min <- lm(sbp ~. ,data = dt %>% select(all_of(names.lasso.min),sbp)
)
summary(modlasso.sbp.min)
pred <- predict(modlasso.sbp.min, newdata = dt %>% select(all_of(names.lasso.min)))
sum((pred-dt$sbp)^2)/nrow(dt)

# With 1se
modlasso.sbp.1se <- lm(sbp ~. ,data = dt %>% select(all_of(names.lasso.1se),sbp)
)
summary(modlasso.sbp.1se)
pred <- predict(modlasso.sbp.1se, newdata = dt %>% select(all_of(names.lasso.1se)))
sum((pred-dt$sbp)^2)/nrow(dt)

## Dyastolic Blood Pressure
## Load variables selected by LASSO
## Load variables selected by LASSO
lasso.coef.min <- read.csv("C:/Users/monic/OneDrive/Desktop/SleepBP/data/lasso_res_dbp_min.csv")
lasso.coef.min$x
names.lasso.min <- c("bmi","total_chol", "hemoglobin", "albumin", "AST",
                     "hypertension", "diabetes", "citizenship_status",
                     "educ_level", "children..5", "psu", "gender", "age_yr",
                     "marital_status", "cycle", "sleep", "race", "snort", 
                     "alcohol")
dt %>% select(all_of(names.lasso.min))

lasso.coef.1se <- read.csv("C:/Users/monic/OneDrive/Desktop/SleepBP/data/lasso_res_dbp_1se.csv")
lasso.coef.1se$x
names.lasso.1se <- c("total_chol", "hypertension", "hemoglobin")
dt %>% select(all_of(names.lasso.1se),sbp)

## Fit lm
## With min
modlasso.dbp.min <- lm(dbp ~. ,data = dt %>% select(all_of(names.lasso.min),dbp)
)
summary(modlasso.dbp.min)
pred <- predict(modlasso.dbp.min, newdata = dt %>% select(all_of(names.lasso.min)))
sum((pred-dt$dbp)^2)/nrow(dt)

# With 1se
modlasso.dbp.1se <- lm(dbp ~. ,data = dt %>% select(all_of(names.lasso.1se),dbp)
)
summary(modlasso.dbp.1se)
pred <- predict(modlasso.dbp.1se, newdata = dt %>% select(all_of(names.lasso.1se)))
sum((pred-dt$dbp)^2)/nrow(dt)








