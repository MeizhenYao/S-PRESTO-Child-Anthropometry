library(parameters)
library(data.table)  
library(readxl)
library(boot)
library(table1)
library(flextable)
library(Rcpp)
library(modelr)
library(lme4)
library(nlme)
library(readr)
library(gtsummary)
library(tidyverse)
library(manipulateWidget)
library(lcmm)
library(stats)
library(boot)
library(parallel)

#-------------------------------------------------------import data set

blood_child_who_stan_updated <- read.csv("~/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_impute_toy4_12182023.csv", header=TRUE)

blood_child_who_stan <- read_excel("~/Projects/S-PRESTO/input/prepared data set/Nate/Nate_PFAS_whole_cohort_20230316.xlsx")

blood_child_who_stan <- blood_child_who_stan %>% 
  select(-matches("Days_since_birth") & -matches("bw_sex") & -matches("bw_birth_GA"))

blood_child_who_stan <- merge(x=blood_child_who_stan_updated, y=blood_child_who_stan, by="Subject_ID", all.x=TRUE)

#------------------------------------------------------- reformate variables
### demographic factors
#### Analysis Batch
blood_child_who_stan$Analysis_Batch_recat <- factor(blood_child_who_stan$Analysis_Batch_recat,
                                                    levels = c("B6","B2","B3","B4","B5"))




#### ethnicity categorization
blood_child_who_stan$ethnicity_specified_recat <- factor(blood_child_who_stan$ethnicity_specified_recat,
                                                         levels=c("Chinese","Malay","Indian"))


#### highest education                            
blood_child_who_stan$pcv1_highest_education_completed_recat <- factor(blood_child_who_stan$pcv1_highest_education_completed_recat,
                                                                      levels=c("University","Primary/Secondary/Post_secondary"))



#### parity status
blood_child_who_stan$pcv1_parity_recat <- factor(blood_child_who_stan$pcv1_parity_recat,
                                                 levels=c("0",">= 1"))

blood_child_who_stan$bw_sex <- factor(blood_child_who_stan$bw_sex,
                                      levels=c("1","2"))


#### Smoking status
for(i in 1:nrow(blood_child_who_stan)){
  if(blood_child_who_stan$pcv1_smoker[i] == 2){
    blood_child_who_stan$pcv1_smoker[i] <- 1
  }
}

blood_child_who_stan$pcv1_smoker <- factor(blood_child_who_stan$pcv1_smoker, levels=c("0", "1"))

blood_child_who_stan$Subject_ID = as.factor(blood_child_who_stan$Subject_ID)
blood_child_who_stan$Subject_ID2 = as.numeric(blood_child_who_stan$Subject_ID) #CHANGES TO NUMERICAL ORDER NOT ACTUALLY THE LEVEL ANEM!

blood_child_who_stan_updated$Subject_ID = as.factor(blood_child_who_stan_updated$Subject_ID)
blood_child_who_stan_updated$Subject_ID2 = as.numeric(blood_child_who_stan_updated$Subject_ID) #CHANGES TO NUMERICAL ORDER NOT ACTUALLY THE LEVEL ANEM!

blood_child_who_stan_age_long <- blood_child_who_stan %>% 
  select(Subject_ID2, Analysis_Batch_recat, age_at_recruitment, pcv1_highest_education_completed_recat,
         ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker, pcv1_bmi, pgv1_bmi, bw_sex, PFOS_Total_quar_num, 
         PFOA_Linear_quar_num, PFNA_quar_num, PFDA_quar_num, PFHpA_quar_num, PFHpS_quar_num, PFHxS_quar_num, 
         PFOS_Total, PFOA_Linear, PFNA, PFDA, PFHpA, PFHpS, PFHxS, SP_CWH11AGE_merge, SP_CWH14AGE, SP_CWH15AGE, SP_CWH16AGE,        
         SP_CWH17AGE, SP_CWH19AGE, SP_CWH20AGE, SP_CWH21AGE, SP_CWH22AGE, SP_CWH23AGE)   %>% 
  pivot_longer(
    cols = starts_with("SP_CWH"),
    names_to = "visit",
    names_prefix = "Days_since_birth_",
    values_to = "Days_since_birth",
    values_drop_na = FALSE
  )

### Rename the visits ###
for(i in 1:nrow(blood_child_who_stan_age_long)){
  if(blood_child_who_stan_age_long$visit[i] == "SP_CWH11AGE_merge"){
    blood_child_who_stan_age_long$visit[i] <- "del"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH14AGE"){
    blood_child_who_stan_age_long$visit[i] <- "wk3"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH15AGE"){
    blood_child_who_stan_age_long$visit[i] <- "wk6"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH16AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m3"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH17AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m6"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH19AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m12"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH20AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m18"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH21AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m24"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH22AGE"){
    blood_child_who_stan_age_long$visit[i] <- "m36"
  }
  else if(blood_child_who_stan_age_long$visit[i] == "SP_CWH23AGE"){
    blood_child_who_stan_age_long$visit[i] <- "y4"
  }
}

blood_child_who_stan_zwfl_long <- blood_child_who_stan_updated %>% 
  select(Subject_ID2, starts_with("zwfl_internal_")) %>% 
  pivot_longer(
    cols = starts_with("zwfl_internal_"),
    names_to = "visit",
    names_prefix = "zwfl_internal_",
    values_to = "zwfl_internal",
    values_drop_na = FALSE
  )

for(i in 1:nrow(blood_child_who_stan_zwfl_long)){
  if(blood_child_who_stan_zwfl_long$visit[i] == "del_merge"){
    blood_child_who_stan_zwfl_long$visit[i] <- "del"
  }
}

blood_child_who_stan_zwfl_long <- as.data.frame(blood_child_who_stan_zwfl_long %>% 
                                                  left_join(blood_child_who_stan_age_long, by = c("Subject_ID2", "visit")))

blood_child_who_stan_zbmi_long <- blood_child_who_stan_updated %>% 
  select(Subject_ID2, starts_with("zbmi_")) %>% 
  pivot_longer(
    cols = starts_with("zbmi_"),
    names_to = "visit",
    names_prefix = "zbmi_",
    values_to = "zbmi",
    values_drop_na = FALSE
  )

for(i in 1:nrow(blood_child_who_stan_zbmi_long)){
  if(blood_child_who_stan_zbmi_long$visit[i] == "del_merge"){
    blood_child_who_stan_zbmi_long$visit[i] <- "del"
  }
}

blood_child_who_stan_zbmi_long <- as.data.frame(blood_child_who_stan_zbmi_long %>% 
                                                  left_join(blood_child_who_stan_age_long, by = c("Subject_ID2", "visit")))


blood_child_who_stan_cbmi_long <- blood_child_who_stan_updated %>% 
  select(Subject_ID2, starts_with("cbmi_")) %>% 
  pivot_longer(
    cols = starts_with("cbmi_"),
    names_to = "visit",
    names_prefix = "cbmi_",
    values_to = "cbmi",
    values_drop_na = FALSE
  )

for(i in 1:nrow(blood_child_who_stan_cbmi_long)){
  if(blood_child_who_stan_cbmi_long$visit[i] == "del_merge"){
    blood_child_who_stan_cbmi_long$visit[i] <- "del"
  }
}

blood_child_who_stan_cbmi_long <- as.data.frame(blood_child_who_stan_cbmi_long %>% 
                                                  left_join(blood_child_who_stan_age_long, by = c("Subject_ID2", "visit")))



blood_child_who_stan_long1 <- merge(x=blood_child_who_stan_zwfl_long, y=blood_child_who_stan_zbmi_long[,c("Subject_ID2", "visit", "zbmi")], 
                                    by=c("Subject_ID2", "visit"))


blood_child_who_stan_long <- merge(x=blood_child_who_stan_long1, y=blood_child_who_stan_cbmi_long[,c("Subject_ID2", "visit", "cbmi")], 
                                   by=c("Subject_ID2", "visit"))


blood_child_who_stan_long$Years_since_birth <- blood_child_who_stan_long$Days_since_birth/365.25

pfas_subjects <- blood_child_who_stan_long[seq(1, nrow(blood_child_who_stan_long), 10),
                                           c("Subject_ID2", "PFOS_Total", "PFOA_Linear", "PFNA", "PFDA", "PFHpA", "PFHpS", "PFHxS")]

pfas_subjects$PFOS_Total_quar_num <- pfas_subjects$PFOS_Total

PFOS_Total_quar <- quantile(pfas_subjects$PFOS_Total, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFOS_Total[i])==TRUE){
    pfas_subjects$PFOS_Total_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[1])){
    pfas_subjects$PFOS_Total_quar_num[i]<-1
  }
  else if (pfas_subjects$PFOS_Total[i] > as.numeric(PFOS_Total_quar[1]) & pfas_subjects$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[2])){
    pfas_subjects$PFOS_Total_quar_num[i]<-2
  }
  else if (pfas_subjects$PFOS_Total[i] > as.numeric(PFOS_Total_quar[2]) & pfas_subjects$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[3])){
    pfas_subjects$PFOS_Total_quar_num[i]<-3
  }
  else if (pfas_subjects$PFOS_Total[i] > as.numeric(PFOS_Total_quar[3])){
    pfas_subjects$PFOS_Total_quar_num[i]<-4
  }
}

pfas_subjects$PFOA_Linear_quar_num <- pfas_subjects$PFOA_Linear

PFOA_Linear_quar <- quantile(pfas_subjects$PFOA_Linear, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFOA_Linear[i])==TRUE){
    pfas_subjects$PFOA_Linear_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[1])){
    pfas_subjects$PFOA_Linear_quar_num[i]<-1
  }
  else if (pfas_subjects$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[1]) & pfas_subjects$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[2])){
    pfas_subjects$PFOA_Linear_quar_num[i]<-2
  }
  else if (pfas_subjects$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[2]) & pfas_subjects$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[3])){
    pfas_subjects$PFOA_Linear_quar_num[i]<-3
  }
  else if (pfas_subjects$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[3])){
    pfas_subjects$PFOA_Linear_quar_num[i]<-4
  }
}

pfas_subjects$PFNA_quar_num <- pfas_subjects$PFNA

PFNA_quar <- quantile(pfas_subjects$PFNA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFNA[i])==TRUE){
    pfas_subjects$PFNA_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFNA[i] <= as.numeric(PFNA_quar[1])){
    pfas_subjects$PFNA_quar_num[i]<-1
  }
  else if (pfas_subjects$PFNA[i] > as.numeric(PFNA_quar[1]) & pfas_subjects$PFNA[i] <= as.numeric(PFNA_quar[2])){
    pfas_subjects$PFNA_quar_num[i]<-2
  }
  else if (pfas_subjects$PFNA[i] > as.numeric(PFNA_quar[2]) & pfas_subjects$PFNA[i] <= as.numeric(PFNA_quar[3])){
    pfas_subjects$PFNA_quar_num[i]<-3
  }
  else if (pfas_subjects$PFNA[i] > as.numeric(PFNA_quar[3])){
    pfas_subjects$PFNA_quar_num[i]<-4
  }
}

pfas_subjects$PFDA_quar_num <- pfas_subjects$PFDA

PFDA_quar <- quantile(pfas_subjects$PFDA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFDA[i])==TRUE){
    pfas_subjects$PFDA_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFDA[i] <= as.numeric(PFDA_quar[1])){
    pfas_subjects$PFDA_quar_num[i]<-1
  }
  else if (pfas_subjects$PFDA[i] > as.numeric(PFDA_quar[1]) & pfas_subjects$PFDA[i] <= as.numeric(PFDA_quar[2])){
    pfas_subjects$PFDA_quar_num[i]<-2
  }
  else if (pfas_subjects$PFDA[i] > as.numeric(PFDA_quar[2]) & pfas_subjects$PFDA[i] <= as.numeric(PFDA_quar[3])){
    pfas_subjects$PFDA_quar_num[i]<-3
  }
  else if (pfas_subjects$PFDA[i] > as.numeric(PFDA_quar[3])){
    pfas_subjects$PFDA_quar_num[i]<-4
  }
}

pfas_subjects$PFHpA_quar_num <- pfas_subjects$PFHpA

PFHpA_quar <- quantile(pfas_subjects$PFHpA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFHpA[i])==TRUE){
    pfas_subjects$PFHpA_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFHpA[i] <= as.numeric(PFHpA_quar[1])){
    pfas_subjects$PFHpA_quar_num[i]<-1
  }
  else if (pfas_subjects$PFHpA[i] > as.numeric(PFHpA_quar[1]) & pfas_subjects$PFHpA[i] <= as.numeric(PFHpA_quar[2])){
    pfas_subjects$PFHpA_quar_num[i]<-2
  }
  else if (pfas_subjects$PFHpA[i] > as.numeric(PFHpA_quar[2]) & pfas_subjects$PFHpA[i] <= as.numeric(PFHpA_quar[3])){
    pfas_subjects$PFHpA_quar_num[i]<-3
  }
  else if (pfas_subjects$PFHpA[i] > as.numeric(PFHpA_quar[3])){
    pfas_subjects$PFHpA_quar_num[i]<-4
  }
}

pfas_subjects$PFHpS_quar_num <- pfas_subjects$PFHpS

PFHpS_quar <- quantile(pfas_subjects$PFHpS, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFHpS[i])==TRUE){
    pfas_subjects$PFHpS_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFHpS[i] <= as.numeric(PFHpS_quar[1])){
    pfas_subjects$PFHpS_quar_num[i]<-1
  }
  else if (pfas_subjects$PFHpS[i] > as.numeric(PFHpS_quar[1]) & pfas_subjects$PFHpS[i] <= as.numeric(PFHpS_quar[2])){
    pfas_subjects$PFHpS_quar_num[i]<-2
  }
  else if (pfas_subjects$PFHpS[i] > as.numeric(PFHpS_quar[2]) & pfas_subjects$PFHpS[i] <= as.numeric(PFHpS_quar[3])){
    pfas_subjects$PFHpS_quar_num[i]<-3
  }
  else if (pfas_subjects$PFHpS[i] > as.numeric(PFHpS_quar[3])){
    pfas_subjects$PFHpS_quar_num[i]<-4
  }
}

pfas_subjects$PFHxS_quar_num <- pfas_subjects$PFHxS

PFHxS_quar <- quantile(pfas_subjects$PFHxS, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(pfas_subjects)){
  if (is.na(pfas_subjects$PFHxS[i])==TRUE){
    pfas_subjects$PFHxS_quar_num[i]<-NA
  }
  else if (pfas_subjects$PFHxS[i] <= as.numeric(PFHxS_quar[1])){
    pfas_subjects$PFHxS_quar_num[i]<-1
  }
  else if (pfas_subjects$PFHxS[i] > as.numeric(PFHxS_quar[1]) & pfas_subjects$PFHxS[i] <= as.numeric(PFHxS_quar[2])){
    pfas_subjects$PFHxS_quar_num[i]<-2
  }
  else if (pfas_subjects$PFHxS[i] > as.numeric(PFHxS_quar[2]) & pfas_subjects$PFHxS[i] <= as.numeric(PFHxS_quar[3])){
    pfas_subjects$PFHxS_quar_num[i]<-3
  }
  else if (pfas_subjects$PFHxS[i] > as.numeric(PFHxS_quar[3])){
    pfas_subjects$PFHxS_quar_num[i]<-4
  }
}

blood_child_who_stan_long <- subset(blood_child_who_stan_long, 
                                    select=-c(PFOS_Total_quar_num, PFOA_Linear_quar_num, PFNA_quar_num, 
                                              PFDA_quar_num, PFHpA_quar_num, PFHpS_quar_num, PFHxS_quar_num))

pfas_subjects <- subset(pfas_subjects, select=-c(PFOS_Total, PFOA_Linear, PFNA, PFDA, PFHpA, PFHpS, PFHxS))

blood_child_who_stan_long <- merge(x=blood_child_who_stan_long, y=pfas_subjects, by="Subject_ID2", all.x=TRUE)

### Drop covariates and merge back in so we can fill in the missing values??????
blood_child_who_stan_long <- subset(blood_child_who_stan_long, 
                                    select=-c(Analysis_Batch_recat, age_at_recruitment, pcv1_highest_education_completed_recat, 
                                              ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker, pcv1_bmi, pgv1_bmi, bw_sex))

covariate_data <- subset(blood_child_who_stan, select=c(Subject_ID2, Analysis_Batch_recat, age_at_recruitment,
                                                        pcv1_highest_education_completed_recat, ethnicity_specified_recat, 
                                                        pcv1_parity_recat, pcv1_smoker, pcv1_bmi, pgv1_bmi, bw_sex))

blood_child_who_stan_long <- merge(x=blood_child_who_stan_long, y=covariate_data, by="Subject_ID2", all.x=TRUE)

### Impute missing BMI for the one subject
for(i in 1:nrow(blood_child_who_stan_long)){
  if(is.na(blood_child_who_stan_long$pcv1_bmi[i])){
    blood_child_who_stan_long$pcv1_bmi[i] <- blood_child_who_stan_long$pgv1_bmi[i]
  }
}

blood_child_who_stan_long$visit <- factor(blood_child_who_stan_long$visit, 
                                          levels=c("del", "wk3", "wk6", "m3", "m6", "m12", "m18", "m24", "m36", "y4"))


### categorize PFAS
blood_child_who_stan_long$PFOS_Total_quar_num<- factor(blood_child_who_stan_long$PFOS_Total_quar_num,
                                                       levels = 1:4)
blood_child_who_stan_long$PFOA_Linear_quar_num<- factor(blood_child_who_stan_long$PFOA_Linear_quar_num,
                                                        levels = 1:4)
blood_child_who_stan_long$PFDA_quar_num<- factor(blood_child_who_stan_long$PFDA_quar_num,
                                                 levels = 1:4)
blood_child_who_stan_long$PFNA_quar_num<- factor(blood_child_who_stan_long$PFNA_quar_num,
                                                 levels = 1:4)
blood_child_who_stan_long$PFHxS_quar_num<- factor(blood_child_who_stan_long$PFHxS_quar_num,
                                                  levels = 1:4)
blood_child_who_stan_long$PFHpS_quar_num<- factor(blood_child_who_stan_long$PFHpS_quar_num,
                                                  levels = 1:4)
blood_child_who_stan_long$PFHpA_quar_num<- factor(blood_child_who_stan_long$PFHpA_quar_num,
                                                  levels = 1:4)




##############################################################################################################################
## Example: PFHxS


### model selection step1: model using random intercept and random slope
blood_child_who_stan_long$age1<- blood_child_who_stan_long$Years_since_birth
blood_child_who_stan_long$age2<- (blood_child_who_stan_long$Years_since_birth)^2
blood_child_who_stan_long$age3<- (blood_child_who_stan_long$Years_since_birth)^3

blood_child_who_stan_long$age3.0.5<- (blood_child_who_stan_long$Years_since_birth-0.5)^3*I(blood_child_who_stan_long$Years_since_birth >0.5)


### unadjusted model

model1<- lme(cbmi ~ age1 + age2+ age3 + age3.0.5,
              random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)

AIC(model1)
summary(model1)

f1 <- function(t) {27.02 - 2*54.54*t + 3*36.31*t^2 - 3*36.29*(t-0.5)^2*I(t>0.5)} 
uniroot(f1, interval=c(0, 1))

# [1] 0.4493


### adjusted model
#### whole model
model2<- lme(cbmi ~  Analysis_Batch_recat + age_at_recruitment +
                pcv1_highest_education_completed_recat + ethnicity_specified_recat +
                pcv1_parity_recat + pcv1_smoker + pcv1_bmi + PFHxS_quar_num +
                age1 + age2+ age3 +age3.0.5,
                random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)

AIC(model2)
summary(model2)

coef2<- model2$coefficients$fixed


f1 <- function(t) {coef2[which(names(coef2)=="age1")] + 2*t*coef2[which(names(coef2)=="age2")] + 3*t^2*coef2[which(names(coef2)=="age3")] + 3*(t-0.5)^2*I(t>0.5)*coef2[which(names(coef2)=="age3.0.5")]} 
uniroot(f1, interval=c(0, 1))
# 0.4487





#### interaction model (pfhxs)
model<- lme(cbmi ~  Analysis_Batch_recat + age_at_recruitment +
                pcv1_highest_education_completed_recat + ethnicity_specified_recat +
                pcv1_parity_recat + pcv1_smoker + pcv1_bmi +
                PFHxS_quar_num*(age1 + age2+ age3 +age3.0.5),
                random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)

AIC(model)
summary(model)


coef<- model$coefficients$fixed

model$coefficients$random

# 1. age at zenith
## at PFAS level 1: 
f1 <- function(t) {coef[which(names(coef)=="age1")] + 2*t*coef[which(names(coef)=="age2")] + 3*t^2*coef[which(names(coef)=="age3")] + 3*(t-0.5)^2*I(t>0.5)*coef[which(names(coef)=="age3.0.5")]} 
uniroot(f1, interval=c(0, 1))


## at PFAS level 2:
f1 <- function(t) {(coef[which(names(coef)=="age1")] + coef[which(names(coef)=="PFHxS_quar_num2:age1")]) + 2*t*(coef[which(names(coef)=="age2")]+ coef[which(names(coef)=="PFHxS_quar_num2:age2")]) + 3*t^2*(coef[which(names(coef)=="age3")]+ coef[which(names(coef)=="PFHxS_quar_num2:age3")]) + 3*(t-0.5)^2*I(t>0.5)*(coef[which(names(coef)=="age3.0.5")]+ coef[which(names(coef)=="PFHxS_quar_num2:age3.0.5")])} 
uniroot(f1, interval=c(0, 1))

## at PFAS level 3:
f1 <- function(t) {(coef[which(names(coef)=="age1")] + coef[which(names(coef)=="PFHxS_quar_num3:age1")]) + 2*t*(coef[which(names(coef)=="age2")]+ coef[which(names(coef)=="PFHxS_quar_num3:age2")]) + 3*t^2*(coef[which(names(coef)=="age3")]+ coef[which(names(coef)=="PFHxS_quar_num3:age3")]) + 3*(t-0.5)^2*I(t>0.5)*(coef[which(names(coef)=="age3.0.5")]+ coef[which(names(coef)=="PFHxS_quar_num3:age3.0.5")])} 
uniroot(f1, interval=c(0, 1))

## at PFAS level 4:
f1 <- function(t) {(coef[which(names(coef)=="age1")] + coef[which(names(coef)=="PFHxS_quar_num4:age1")]) + 2*t*(coef[which(names(coef)=="age2")]+ coef[which(names(coef)=="PFHxS_quar_num4:age2")]) + 3*t^2*(coef[which(names(coef)=="age3")]+ coef[which(names(coef)=="PFHxS_quar_num4:age3")]) + 3*(t-0.5)^2*I(t>0.5)*(coef[which(names(coef)=="age3.0.5")]+ coef[which(names(coef)=="PFHxS_quar_num4:age3.0.5")])} 
uniroot(f1, interval=c(0, 1))




bmi_zenith <- coef[which(names(coef)=="(Intercept)")] + coef[which(names(coef)=="age1")] * zenith_age + beta2 * zenith_age^2 + beta3 * zenith_age^3 + gamma3 * (zenith_age - t0)^3


bmi_zenith <- beta0 + b0i + (beta1 + b1i) * zenith_age + beta2 * zenith_age^2 + beta3 * zenith_age^3 + gamma3 * (zenith_age - t0)^3



## 2. bmi at zenith
age1<- c(rep(0.4493, table(blood_child_who_stan_long$PFHxS_quar_num)[1]/10), rep(0.4366, table(blood_child_who_stan_long$PFHxS_quar_num)[2]/10),
         rep(0.4367, table(blood_child_who_stan_long$PFHxS_quar_num)[3]/10), rep(0.6497, table(blood_child_who_stan_long$PFHxS_quar_num)[4]/10))
age2 <- age1^2 
age3 <- age1^3 
age3.0.5<-(age1-0.5)^3*I(age1>0.5) 
covariates<- blood_child_who_stan_long %>% 
  filter(visit == "m6") %>% 
  arrange(by = PFHxS_quar_num) %>% 
  select(Subject_ID2, PFHxS_quar_num, Analysis_Batch_recat, age_at_recruitment, pcv1_highest_education_completed_recat,
         ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker, pcv1_bmi)


data_pred<- data.frame(age1, age2, age3, age3.0.5, covariates) 

data_pred$pred <- predict(model, newdata= data_pred, level=0) 



mean(data_pred[data_pred$PFHxS_quar_num==1,]$pred)
mean(data_pred[data_pred$PFHxS_quar_num==2,]$pred)
mean(data_pred[data_pred$PFHxS_quar_num==3,]$pred)
mean(data_pred[data_pred$PFHxS_quar_num==4,]$pred)


























with(data_pred[data_pred$PFHxS_quar_num==1,], plot(age1, pred, type="p", lty=1, lwd=2, ylab="cbmi", xlab="Age (years)", col="#D7191C"))  
with(data_pred[data_pred $PFHxS_quar_num ==2,], lines(age1, pred, type="p", lwd=2, col="#FDAE61")) 
with(data_pred [data_pred $PFHxS_quar_num ==3,], lines(age1, pred, type="p", lwd=2, col="#ABDDA4")) 
with(data_pred [data_pred $PFHxS_quar_num ==4,], lines(age1, pred, type="p", lwd=2, col="#2B83BA")) 
with(data_pred, rug(age1)) 

mean(data_pred[data_pred$PFHxS_quar_num==1,]$pred)





## age difference

## bmi difference






