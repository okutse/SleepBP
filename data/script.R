#############################
## Sleep and Blood Pressure #
## Amos Okutse              #
## Monica Colon-Vargas      #
#############################

## load required libraries
library(nhanesA)
library(knitr)
library(kableExtra)
library(tidyverse)
library(gtsummary)
library(survey)

## load the 2015 cycle and add an indicator of survey cycle
load("../ProjectFinal/data/nhanes2015.RData")
load("../ProjectFinal/data/nhanes2017.RData")
nhanes2015$cycle <- 0
nhanes2017$cycle <- 1

## check what names do not match between the two surveys
setdiff(names(nhanes2015), names(nhanes2017))

## only column ALQ120Q is not the same and we can change that label in the 2015 cycle to match and merge
nhanes2015 <- nhanes2015 %>% 
  dplyr::rename(ALQ121 = ALQ120Q)

## we can confirm that the names are now identical
identical(names(nhanes2015), names(nhanes2017))

## append the two data files together
df <- rbind(nhanes2015, nhanes2017)

## get the dimension of combined file
dim(df)

## rename variable names
dt <- df %>% 
  dplyr::rename("seq_no" = SEQN, 
                "psu" = SDMVPSU, 
                "strata" = SDMVSTRA, 
                "gender" = RIAGENDR, 
                "age_yr" = RIDAGEYR, 
                "marital_status" = DMDMARTL, 
                "weights" = WTINT2YR, 
                "income_range" = INDFMIN2, 
                "race" = RIDRETH3, 
                "children <5" = DMDHHSZA, 
                "educ_level" = DMDEDUC2, 
                "citizenship_status" = DMDCITZN, 
                "sleep" = SLD012, 
                "bmi" = BMXBMI, 
                "alcohol" = ALQ121, 
                "hdl" = LBDHDDSI, 
                "albumin" = LBDSALSI, 
                "diabetes" = DIQ010, 
                "hypertension" = BPQ020, 
                "creatinine" = URXCRS, 
                "snort" = SLQ040, 
                "total_chol" = LBDTCSI,
                "hemoglobin" = LBXHGB, 
                "AST" = LBXSASSI, 
                "smoke" = SMQ040, 
                "hyper_meds" = BPQ050A)

## Table of questions used when extracting variables from the NHANES database


## drop missing sleep values
dt <- dt %>% tidyr::drop_na(sleep)
dim(dt)

## combine the systolic and diastolic blood pressure by averaging the four measurements available
dt$dbp <- rowMeans(subset(dt, select = c(BPXDI1, BPXDI2, BPXDI3, BPXDI4)), na.rm = TRUE)
dt$sbp <- rowMeans(subset(dt, select = c(BPXSY1, BPXSY2, BPXSY3, BPXSY4)), na.rm = TRUE)

## we can then delete the variables related to dbp and sbp from the data set and drop missing values for either dbp or sbp
dt <- dt %>% 
  dplyr::select(-c(BPXDI1, BPXDI2, BPXDI3, BPXDI4, BPXSY1, BPXSY2, BPXSY3, BPXSY4)) %>% 
  tidyr::drop_na(dbp, sbp)
dim(dt)

## then we exclude individuals on antihypertension meds
dt = dt %>% 
  dplyr::filter(is.na(hyper_meds)|hyper_meds == "No")
dim(dt)

## we also need to exclude individuals with missing BMI and then finally subset to only underweight or obese
dt <- dt %>% tidyr::drop_na(bmi) %>% 
  dplyr::filter(bmi < 25)
dim(dt)

## sleep duration
dt$sleep2 <- as.factor(ifelse(dt$sleep < 6, "<6hrs", 
                              ifelse((dt$sleep >= 6) & (dt$sleep < 8), "6-8hrs", ">8hrs")))
#race
table(dt$race)
dt$race2 <- as.factor(ifelse(dt$race == "Mexican American", "Mexican American",
                             ifelse(dt$race == "Non-Hispanic White", "White",
                                    ifelse(dt$race == "Non-Hispanic Black", "Black", "Other"))))
## smoking
dt$smoke2 <- as.factor(ifelse(dt$smoke == "Every day" | dt$smoke == "Some days", "Smoking",
                              ifelse(dt$smoke == "Not at all", "Not smoking", "Not recorded")))

## diabetes
table(dt$diabetes)

## hypertension
table(dt$hypertension)

## snort
table(dt$snort)
dt$snort2 <- as.factor(ifelse(dt$snort == "Never", "No",
                              ifelse(dt$snort == "Rarely - 1-2 nights a week"| dt$snort == "Occasionally - 3-4 nights a week" | dt$snort == "Frequently - 5 or more nights a", "Yes", "Not recorded")))

## gender
table(dt$gender)

## alcohol consumption
table(dt$alcohol, useNA = "ifany")
dt[dt == 999] <- NA ## convert missing to NA
## assume individuals with 0 drinks have never drank and all others with info have drank, and then we have NA
dt$alcohol2 <- as.factor(ifelse(dt$alcohol == 0, "No drinking", 
                                ifelse(is.na(dt$alcohol), "Not recorded", "Drinking")))
table(dt$alcohol2)

## select the variables in Table 1
dt2 <- dt %>% dplyr::select(age_yr, albumin, creatinine, sbp, dbp, hemoglobin, total_chol, AST, hdl, bmi, alcohol2, diabetes, hypertension, snort2, smoke2, race2, sleep2, gender)
#str(dt2)

table1 <- dt2 %>% gtsummary::tbl_summary(by = gender,
                                         statistic = list(all_continuous() ~ "{mean} ({sd})", 
                                                          all_categorical() ~ "{p}"),
                                         percent = "column",
                                         missing = "no") %>% 
  bold_labels() %>% 
  add_overall() %>% 
  add_p() %>% 
  as_kable(format = "pipe", caption = "Baseline participant characteristics", digits = 2) #%>% 
#kable_styling(latex_options = "scale_down")
table1