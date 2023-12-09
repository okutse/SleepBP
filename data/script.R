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
library(leaps)

## load the 2015 cycle and add an indicator of survey cycle
load("nhanes2015.RData")
load("nhanes2017.RData")
nhanes2015$cycle <- 0
nhanes2017$cycle <- 1


nhanes2015 %>% head()
nhanes2017 %>% head()

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

tbl_one <- data.frame(Variable = as.character(), Name = as.character(), Description = as.character()) %>% 
  add_row(Variable = "SEQN", Name = "sequence number", Description = "Respondent sequence number") %>% 
  add_row(Variable = "SDMVPSU", Name = "psu", Description = "Masked variance unit pseudo-PSU variable for variance estimation") %>% 
  add_row(Variable = "SDMVSTRA", Name = "strata", Description = "Masked variance unit pseudo-stratum variable for variance estimation") %>% 
  add_row(Variable = "RIAGENDR", Name = "gender", Description = "Gender of the participant") %>% 
  add_row(Variable = "", )
  


## Check missing values per variable
#Count missing data across each variable
missing_table <- dt %>% 
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  t() %>% 
  as.data.frame() %>%
  mutate(missing = V1) %>%
  select(missing) %>% mutate(percent = round(missing/dim(dt)[1],4)*100)

missing_table %>%
  filter(missing > 0) %>%
  kable(caption = "Count of Missing Data across all variables",
        booktabs=T,
        col.names = c("Number of Missing","Percent")) %>% 
  kable_styling(full_width=T, latex_options = c('HOLD_position'))

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

## Then we exclude individuals on antihypertension meds
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
#dt$smoke2 <- as.factor(ifelse(dt$smoke == "Every day" | dt$smoke == "Some days", "Smoking",
#                              ifelse(dt$smoke == "Not at all", "Not smoking", "Not recorded")))
dt <- dt %>%
  mutate(smoke2 = case_when(smoke == "Every day" ~ "Smoking",
                            smoke == "Some days" ~ "Smoking",
                            smoke == "Not at all" ~ "Not Smoking"))

dt$smoke2 <- ifelse(is.na(dt$smoke)==T,"Not recorded", dt$smoke2)
dt$smoke2 <- as.factor(dt$smoke2)


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
dt$alcohol2 <- ifelse(dt$alcohol == 0, "No drinking", "Drinking")
dt$alcohol2 <- ifelse(is.na(dt$alcohol2),"Not recorded",dt$alcohol2)
dt$alcohol2 <- as.factor(dt$alcohol2)



table(dt$alcohol2)

## select the variables in Table 1
dt2 <- dt %>% dplyr::select(age_yr, albumin, creatinine, sbp, dbp, hemoglobin, total_chol, AST, hdl, bmi, alcohol2, diabetes, hypertension, snort2, smoke2, race2, sleep2, gender)
str(dt2)

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

#age alcohol sleep and smoking status

library(tidyverse)
library(naniar)
library(ggcorrplot)
library(GGally)
library(ggpubr)
library(glmnet)

vis_miss(dt)
dt <- dt %>% select(-hyper_meds,-sleep,-race,-smoke,-snort,-alcohol)
dt$cycle <- as.factor(dt$cycle)
dt <- dt %>%
  rename(sleep = sleep2,
         race = race2,
         smoke = smoke2,
         snort = snort2,
         alcohol = alcohol2)


mod1 <- lm(dbp ~. ,data = dt)
summary(mod1)

#COMPLETE CASE ANALYSIS
#After removing some vairables and filtering some NAs and Changin NA to not recorded
dt <- na.omit(dt)

dt


## declare data as survey data
design = svydesign(id = ~psu, weights = ~weights, strata = ~strata, 
          nest = TRUE, survey.lonely.psu = "adjust", data = dt)
mod <- svyglm()
View(dt)

##EXPLORATORY DATA ANALYSIS
ggcorrplot(cor(dt %>% select_if(is.numeric)))
names_cont <- dt %>% select_if(is.numeric) %>% colnames()
names_cat <- dt %>% select_if(is.factor) %>% colnames()

p1 <- ggplot(dt, aes(x = bmi, y = dbp))+geom_point()+theme_bw()+ggtitle('BMI')
p2 <- ggplot(dt, aes(x = hdl, y = dbp))+geom_point()+theme_bw()+ggtitle("hdl")
p3 <- ggplot(dt, aes(x = total_chol, y = dbp))+geom_point()+theme_bw()+ggtitle("total_chol")
p4 <- ggplot(dt, aes(x = hemoglobin, y = dbp))+geom_point()+theme_bw()+ggtitle("hemoglobin")
p5 <- ggplot(dt, aes(x = AST, y = dbp))+geom_point()+theme_bw()+ggtitle("AST")
p6 <- ggplot(dt, aes(x = albumin, y = dbp))+geom_point()+theme_bw()+ggtitle("albumin")
p7 <- ggplot(dt, aes(x = strata, y = dbp))+geom_point()+theme_bw()+ggtitle("strata")
p8 <- ggplot(dt, aes(x = age_yr, y = dbp))+geom_point()+theme_bw()+ggtitle("age_yr")
p9 <- ggplot(dt, aes(x = weights, y = dbp))+geom_point()+theme_bw()+ggtitle("weights")
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol = 3,nrow = 3)

p1 <- ggplot(dt, aes(x = bmi, y = dbp))+geom_boxplot()+theme_bw()+ggtitle('BMI')
p2 <- ggplot(dt, aes(x = hdl, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("hdl")
p3 <- ggplot(dt, aes(x = total_chol, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("total_chol")
p4 <- ggplot(dt, aes(x = hemoglobin, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("hemoglobin")
p5 <- ggplot(dt, aes(x = AST, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("AST")
p6 <- ggplot(dt, aes(x = albumin, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("albumin")
p7 <- ggplot(dt, aes(x = strata, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("strata")
p8 <- ggplot(dt, aes(x = age_yr, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("age_yr")
p9 <- ggplot(dt, aes(x = weights, y = dbp))+geom_boxplot()+theme_bw()+ggtitle("weights")
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol = 3,nrow = 3)


p1 <- ggplot(dt, aes(y = bmi))+geom_boxplot()+theme_bw()+ggtitle('BMI')
p2 <- ggplot(dt, aes(y = hdl))+geom_boxplot()+theme_bw()+ggtitle("hdl")
p3 <- ggplot(dt, aes(y = total_chol))+geom_boxplot()+theme_bw()+ggtitle("total_chol")
p4 <- ggplot(dt, aes(y = hemoglobin))+geom_boxplot()+theme_bw()+ggtitle("hemoglobin")
p5 <- ggplot(dt, aes(y = AST))+geom_boxplot()+theme_bw()+ggtitle("AST")
p6 <- ggplot(dt, aes(y = albumin))+geom_boxplot()+theme_bw()+ggtitle("albumin")
p7 <- ggplot(dt, aes(y = strata))+geom_boxplot()+theme_bw()+ggtitle("strata")
p8 <- ggplot(dt, aes(y = age_yr))+geom_boxplot()+theme_bw()+ggtitle("age_yr")
p9 <- ggplot(dt, aes(y = weights))+geom_boxplot()+theme_bw()+ggtitle("weights")
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol = 3,nrow = 3)
#MANY OUTLIERS

p1 <- ggplot(dt, aes(x = hypertension, y = dbp, fill = hypertension))+geom_boxplot()+theme_bw()
p2 <- ggplot(dt, aes(x = diabetes, y = dbp, fill = diabetes))+geom_boxplot()+theme_bw()
p3 <- ggplot(dt, aes(x = citizenship_status, y = dbp, fill = citizenship_status))+geom_boxplot()+theme_bw()
p4 <- ggplot(dt, aes(x = educ_level, y = dbp, fill = educ_level))+geom_boxplot()+theme_bw()
p5 <- ggplot(dt, aes(x = `children <5`, y = dbp, fill = `children <5`))+geom_boxplot()+theme_bw()
p6 <- ggplot(dt, aes(x = gender, y = dbp, fill = gender))+geom_boxplot()+theme_bw()
p7 <- ggplot(dt, aes(x = income_range, y = dbp, fill = income_range))+geom_boxplot()+theme_bw()
p8 <- ggplot(dt, aes(x = marital_status, y = dbp, fill = marital_status))+geom_boxplot()+theme_bw()
p9 <- ggplot(dt, aes(x = cycle, y = dbp, fill = cycle))+geom_boxplot()+theme_bw()
p10 <- ggplot(dt, aes(x = sleep, y = dbp, fill = sleep))+geom_boxplot()+theme_bw()
p11 <- ggplot(dt, aes(x = race, y = dbp, fill = race))+geom_boxplot()+theme_bw()
p12 <-  ggplot(dt, aes(x = smoke, y = dbp, fill = smoke))+geom_boxplot()+theme_bw()
p13 <-  ggplot(dt, aes(x = snort, y = dbp, fill = snort))+geom_boxplot()+theme_bw()
p14 <-  ggplot(dt, aes(x = alcohol, y = dbp, fill = alcohol))+geom_boxplot()+theme_bw()
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,ncol = 3,nrow = 5,legend = "none")


#join income range to 3 categories: poor rich and med?
#Income ranges are $28,007 or less for the lower class, 
#$55,001 to $89,744 for the middle class,
#and $149,132 or more for the upper class. 


dt$income_range %>% levels()
# Re-categorizing income ranges
dt$income_category <- case_when(
  dt$income_range %in% c("$ 0 to $ 4,999", "$ 5,000 to $ 9,999") ~ "Low income",
  dt$income_range %in% c("$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999") ~ "Lower-middle income",
  dt$income_range %in% c("$25,000 to $34,999", "$35,000 to $44,999", "$45,000 to $54,999") ~ "Middle income",
  dt$income_range %in% c("$55,000 to $64,999", "$65,000 to $74,999", "$75,000 to $99,999") ~ "Upper-middle income",
  dt$income_range %in% c("$20,000 and Over", "Under $20,000", "$100,000 and Over") ~ "Varied/High income",
  dt$income_range %in% c("Refused", "Don't know") ~ "Unknown/Refused",
  TRUE ~ NA_character_  # For any other cases not covered
)
dt <- dt %>% select(-income_range)
dt$income_category <- as.factor(dt$income_category)
table(dt$income_category)
table(dt$educ_level)
dt <- dt %>% 
  mutate(educ_level = case_when(
    educ_level %in% c("Less than 9th grade", "9-11th grade (Includes 12th grad") ~ "Less12grade",
    educ_level %in% c("High school graduate/GED or equi") ~ "Highschool",
    educ_level %in% c("Some college or AA degree") ~ "someCollege",
    educ_level %in% c("College graduate or above") ~ "GraduateStudies",
    educ_level %in% c("Dont know", "Refused") ~ "Unknown/Refused")) %>%
  mutate(educ_level = as.factor(educ_level))

## SAve
#write.csv(dt, file = "C:/Users/monic/OneDrive/Desktop/SleepBP/data", row.names = F)
#write.csv(dt, file = "C:/Users/monic/OneDrive/Desktop/SleepBP/data/dt.csv")

##########DBP

dt <- dt %>%
  mutate_if(is.character,as.factor)

## Set seed for folds
set.seed(123)
folds <- sample(1:10,nrow(dt),replace = T)

## Create the matrix of the model
X <- model.matrix(sbp~., data = dt %>% select(-dbp,-weights,-strata,-seq_no,-psu))[,-1]
y <- dt$sbp

## Do cross validation
lasso.mod.cv <- cv.glmnet(X, y, alpha=1, nfolds = 10, foldid = folds)
lasso.mod.cv$lambda.min
lasso.mod <- glmnet(X,y,alpha = 1,lambda = lasso.mod.cv$lambda.min)

## Plot cross validation results
plot(lasso.mod.cv) #maybe choose one larger lambda that not adds more mse?

## Select nonzero coefficients
coefficients <- coef(lasso.mod)
nonzero.coef <- coefficients[,1] != 0
nonzero.vars.lasso_sbp_min <- rownames(coefficients)[nonzero.coef]

write.csv(nonzero.vars.lasso_sbp_min, file = "C:/Users/monic/OneDrive/Desktop/SleepBP/data/lasso_res_sbp_min.csv")

## Filter dataframe to include only non-zero variables
modified_dt <- dt %>% 
  select(bmi,total_chol,hemoglobin,albumin,AST,hypertension,diabetes, `children <5`, 
         psu, gender, age_yr, marital_status, cycle, race, alcohol, sleep,
         income_category, citizenship_status, snort, educ_level,strata, weights,dbp)
#remove citizenship, educ level, sleep, income category. snort bc only 1?

summary(lm(dbp~. ,data = modified_dt))


#############
##########################################################

# Perform forward subset selection
# Cross-Validation

k <- 10
n <- nrow(dt)
p <- ncol(dt %>% select(-dbp,-weights,-strata,-seq_no, -psu)) -1

set.seed(1)
#folds <- sample(1:k, n, replace=TRUE)

#ISSUES  for stepwise selection 
cv.errors <- matrix(NA,k,p,
                    dimnames = list(NULL, paste(1:p))) #This takes too long, maybe use only variables of lasso??
modified_dt <- dt %>% select(-dbp,-weights,-strata)
for (j in 1:k) {
  best.fit <- regsubsets(sbp ~., 
                         data = modified_dt[folds != j, ],
                         nvmax = p, 
                         really.big = T)
  print(j) 
  for (i in 1:p) {
    coefi <- coef(best.fit, id = i)
    test.mat <- model.matrix(sbp ~., data = modified_dt[folds == j,])
    pred <- test.mat[,names(coefi)] %*% coefi
    cv.errors[j,i]  <- mean((modified_dt[folds == j,]$sbp - pred)^2)
    print(i)
  }
}
errors <- data.frame(subset_size = 1:p, error = colMeans(cv.errors))
errors %>% arrange(error)
best_k <- which.min(errors$error)
best_k_2 <- 14
ggplot(errors, aes(x = subset_size, y = error, color = subset_size == best_k)) +geom_point()+theme_minimal()+
  geom_line()+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))+
  scale_x_continuous(breaks = 1:p)+
  guides(color = "none")+ggtitle("Cross Valiation for Foward Setsize")


## Get coefficients with best_k
reg.best <- regsubsets(sbp~., data = modified_dt, nvmax = p)
summary(reg.best)
coef_reg_bestk_sbp <- coef(reg.best, best_k) #levels of variables but not whole variables
coef_reg_k2_sbp <- coef(reg.best, best_k_2) 
#should we fit a new model like below?
coef_reg_bestk_sbp <- as.data.frame(names(coef_reg_bestk_sbp))
coef_reg_k2_sbp <- as.data.frame(names(coef_reg_k2_sbp))
write.csv(coef_reg_bestk_sbp, file = "C:/Users/monic/OneDrive/Desktop/SleepBP/data/reg_res_sbp_bestk.csv")
write.csv(coef_reg_k2_sbp, file = "C:/Users/monic/OneDrive/Desktop/SleepBP/data/reg_res_sbp_k2.csv")

## Fit model to get summary
mod.regbest <- lm(dbp~ total_chol+hemoglobin+hypertension+race+alcohol,data=modified_dt)
summary(mod.regbest)

###VARIABLE IMPORANCE FOR REGSUBSET

## Extract coefficients and their names
coef_values <- as.data.frame(coef(lasso.mod)[-1,])
coef_values$variables <- rownames(coef_values)# Adding a column with variable names
colnames(coef_values) <- c("V1", "variables")
rownames(coef_values) <- NULL

## Rearrange coefficients by absolute magnitude
coef_values <-coef_values %>%
  arrange(desc(abs(V1))) %>%
  filter(V1!= 0)

coef_values$variables <- factor(coef_values$variables, levels = coef_values$variables)

## Plot coefficients in ascending order of absolute magnitude
ggplot(coef_values, aes(x = variables, y = V1)) +
  geom_bar(stat = "identity") +
  labs(x = "Variables", y = "Variable Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))















###VARIABLE IMPORANCE FOR LASSO

## Extract coefficients and their names
coef_values <- as.data.frame(coef(lasso.mod)[-1,])
coef_values$variables <- rownames(coef_values)# Adding a column with variable names
colnames(coef_values) <- c("V1", "variables")
rownames(coef_values) <- NULL

## Rearrange coefficients by absolute magnitude
coef_values <-coef_values %>%
  arrange(desc(abs(V1))) %>%
  filter(V1!= 0)

coef_values$variables <- factor(coef_values$variables, levels = coef_values$variables)

## Plot coefficients in ascending order of absolute magnitude
ggplot(coef_values, aes(x = variables, y = V1)) +
  geom_bar(stat = "identity") +
  labs(x = "Variables", y = "Variable Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))












##############
#SYSTOLIC
library(leaps)
# Perform forward subset selection
# Cross-Validation

k <- 10
n <- nrow(dt)
p <- ncol(dt %>% select(-dbp,-weights,-strata,-seq_no, -psu,-hypertension)) -1
set.seed(123)
folds <- sample(1:k, n, replace=TRUE)

#ISSUES  for stepwise selection 
cv.errors.sbp <- matrix(NA,k,p,
                    dimnames = list(NULL, paste(1:p))) #This takes too long, maybe use only variables of lasso??
modified_dt <- dt %>% dplyr::select(-dbp,-weights,-strata,-seq_no, -psu, - hypertension)
for (j in 1:k) {
  best.fit <- regsubsets(sbp ~., 
                         data = modified_dt[folds != j, ],
                         nvmax = p, 
                         really.big = T)
  print(j) 
  for (i in 1:p) {
    coefi <- coef(best.fit, id = i)
    test.mat <- model.matrix(sbp ~., data = modified_dt[folds == j,])
    pred <- test.mat[,names(coefi)] %*% coefi
    cv.errors.sbp[j,i]  <- mean((modified_dt[folds == j,]$sbp - pred)^2)
    print(i)
  }
}
errors.sbp <- data.frame(subset_size = 1:p, error = colMeans(cv.errors.sbp))
best_k.sbp <- which.min(errors.sbp$error)
ggplot(errors.sbp, aes(x = subset_size, y = error, color = subset_size == best_k.sbp)) +geom_point()+theme_minimal()+
  geom_line()+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))+
  scale_x_continuous(breaks = 1:p)+
  guides(color = "none")+ggtitle("Cross Valiation for Foward Setsize")


## Get coefficients with best_k
reg.best.sbp <- regsubsets(sbp~., data = modified_dt, nvmax = p)



##########
##DYstolic
k <- 10
n <- nrow(dt)
p <- ncol(dt %>% select(-sbp,-weights,-strata,-seq_no, -psu,-hypertension)) -1
set.seed(123)
folds <- sample(1:k, n, replace=TRUE)

#ISSUES  for stepwise selection 
cv.errors.dbp <- matrix(NA,k,p,
                        dimnames = list(NULL, paste(1:p))) #This takes too long, maybe use only variables of lasso??
modified_dt <- dt %>% dplyr::select(-sbp,-weights,-strata,-seq_no, -psu, - hypertension)
for (j in 1:k) {
  best.fit <- regsubsets(dbp ~., 
                         data = modified_dt[folds != j, ],
                         nvmax = p, 
                         really.big = T)
  print(j) 
  for (i in 1:p) {
    coefi <- coef(best.fit, id = i)
    test.mat <- model.matrix(dbp ~., data = modified_dt[folds == j,])
    pred <- test.mat[,names(coefi)] %*% coefi
    cv.errors.dbp[j,i]  <- mean((modified_dt[folds == j,]$dbp - pred)^2)
    print(i)
  }
}
errors.dbp <- data.frame(subset_size = 1:p, error = colMeans(cv.errors.dbp))
best_k.dbp <- which.min(errors.dbp$error)
ggplot(errors.dbp, aes(x = subset_size, y = error, color = subset_size == best_k.dbp)) +geom_point()+theme_minimal()+
  geom_line()+
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"))+
  scale_x_continuous(breaks = 1:p)+
  guides(color = "none")+ggtitle("Cross Valiation for Foward Setsize")


## Get coefficients with best_k
reg.best.dbp <- regsubsets(dbp~., data = modified_dt, nvmax = p)










