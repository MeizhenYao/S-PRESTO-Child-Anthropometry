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


#### quantile PFAS

blood_child_who_stan$PFOS_Total_quar_num <- blood_child_who_stan$PFOS_Total

PFOS_Total_quar <- quantile(blood_child_who_stan$PFOS_Total, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFOS_Total[i])==TRUE){
    blood_child_who_stan$PFOS_Total_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[1])){
    blood_child_who_stan$PFOS_Total_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFOS_Total[i] > as.numeric(PFOS_Total_quar[1]) & blood_child_who_stan$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[2])){
    blood_child_who_stan$PFOS_Total_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFOS_Total[i] > as.numeric(PFOS_Total_quar[2]) & blood_child_who_stan$PFOS_Total[i] <= as.numeric(PFOS_Total_quar[3])){
    blood_child_who_stan$PFOS_Total_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFOS_Total[i] > as.numeric(PFOS_Total_quar[3])){
    blood_child_who_stan$PFOS_Total_quar_num[i]<-4
  }
}

blood_child_who_stan$PFOA_Linear_quar_num <- blood_child_who_stan$PFOA_Linear

PFOA_Linear_quar <- quantile(blood_child_who_stan$PFOA_Linear, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFOA_Linear[i])==TRUE){
    blood_child_who_stan$PFOA_Linear_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[1])){
    blood_child_who_stan$PFOA_Linear_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[1]) & blood_child_who_stan$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[2])){
    blood_child_who_stan$PFOA_Linear_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[2]) & blood_child_who_stan$PFOA_Linear[i] <= as.numeric(PFOA_Linear_quar[3])){
    blood_child_who_stan$PFOA_Linear_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFOA_Linear[i] > as.numeric(PFOA_Linear_quar[3])){
    blood_child_who_stan$PFOA_Linear_quar_num[i]<-4
  }
}

blood_child_who_stan$PFNA_quar_num <- blood_child_who_stan$PFNA

PFNA_quar <- quantile(blood_child_who_stan$PFNA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFNA[i])==TRUE){
    blood_child_who_stan$PFNA_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFNA[i] <= as.numeric(PFNA_quar[1])){
    blood_child_who_stan$PFNA_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFNA[i] > as.numeric(PFNA_quar[1]) & blood_child_who_stan$PFNA[i] <= as.numeric(PFNA_quar[2])){
    blood_child_who_stan$PFNA_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFNA[i] > as.numeric(PFNA_quar[2]) & blood_child_who_stan$PFNA[i] <= as.numeric(PFNA_quar[3])){
    blood_child_who_stan$PFNA_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFNA[i] > as.numeric(PFNA_quar[3])){
    blood_child_who_stan$PFNA_quar_num[i]<-4
  }
}

blood_child_who_stan$PFDA_quar_num <- blood_child_who_stan$PFDA

PFDA_quar <- quantile(blood_child_who_stan$PFDA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFDA[i])==TRUE){
    blood_child_who_stan$PFDA_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFDA[i] <= as.numeric(PFDA_quar[1])){
    blood_child_who_stan$PFDA_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFDA[i] > as.numeric(PFDA_quar[1]) & blood_child_who_stan$PFDA[i] <= as.numeric(PFDA_quar[2])){
    blood_child_who_stan$PFDA_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFDA[i] > as.numeric(PFDA_quar[2]) & blood_child_who_stan$PFDA[i] <= as.numeric(PFDA_quar[3])){
    blood_child_who_stan$PFDA_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFDA[i] > as.numeric(PFDA_quar[3])){
    blood_child_who_stan$PFDA_quar_num[i]<-4
  }
}

blood_child_who_stan$PFHpA_quar_num <- blood_child_who_stan$PFHpA

PFHpA_quar <- quantile(blood_child_who_stan$PFHpA, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFHpA[i])==TRUE){
    blood_child_who_stan$PFHpA_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFHpA[i] <= as.numeric(PFHpA_quar[1])){
    blood_child_who_stan$PFHpA_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFHpA[i] > as.numeric(PFHpA_quar[1]) & blood_child_who_stan$PFHpA[i] <= as.numeric(PFHpA_quar[2])){
    blood_child_who_stan$PFHpA_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFHpA[i] > as.numeric(PFHpA_quar[2]) & blood_child_who_stan$PFHpA[i] <= as.numeric(PFHpA_quar[3])){
    blood_child_who_stan$PFHpA_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFHpA[i] > as.numeric(PFHpA_quar[3])){
    blood_child_who_stan$PFHpA_quar_num[i]<-4
  }
}

blood_child_who_stan$PFHpS_quar_num <- blood_child_who_stan$PFHpS

PFHpS_quar <- quantile(blood_child_who_stan$PFHpS, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFHpS[i])==TRUE){
    blood_child_who_stan$PFHpS_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFHpS[i] <= as.numeric(PFHpS_quar[1])){
    blood_child_who_stan$PFHpS_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFHpS[i] > as.numeric(PFHpS_quar[1]) & blood_child_who_stan$PFHpS[i] <= as.numeric(PFHpS_quar[2])){
    blood_child_who_stan$PFHpS_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFHpS[i] > as.numeric(PFHpS_quar[2]) & blood_child_who_stan$PFHpS[i] <= as.numeric(PFHpS_quar[3])){
    blood_child_who_stan$PFHpS_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFHpS[i] > as.numeric(PFHpS_quar[3])){
    blood_child_who_stan$PFHpS_quar_num[i]<-4
  }
}

blood_child_who_stan$PFHxS_quar_num <- blood_child_who_stan$PFHxS

PFHxS_quar <- quantile(blood_child_who_stan$PFHxS, c(1/4,1/2,3/4), na.rm=TRUE, type=2)

for(i in 1:nrow(blood_child_who_stan)){
  if (is.na(blood_child_who_stan$PFHxS[i])==TRUE){
    blood_child_who_stan$PFHxS_quar_num[i]<-NA
  }
  else if (blood_child_who_stan$PFHxS[i] <= as.numeric(PFHxS_quar[1])){
    blood_child_who_stan$PFHxS_quar_num[i]<-1
  }
  else if (blood_child_who_stan$PFHxS[i] > as.numeric(PFHxS_quar[1]) & blood_child_who_stan$PFHxS[i] <= as.numeric(PFHxS_quar[2])){
    blood_child_who_stan$PFHxS_quar_num[i]<-2
  }
  else if (blood_child_who_stan$PFHxS[i] > as.numeric(PFHxS_quar[2]) & blood_child_who_stan$PFHxS[i] <= as.numeric(PFHxS_quar[3])){
    blood_child_who_stan$PFHxS_quar_num[i]<-3
  }
  else if (blood_child_who_stan$PFHxS[i] > as.numeric(PFHxS_quar[3])){
    blood_child_who_stan$PFHxS_quar_num[i]<-4
  }
}




### Impute missing BMI for the one subject
for(i in 1:nrow(blood_child_who_stan)){
  if(is.na(blood_child_who_stan$pcv1_bmi[i])){
    blood_child_who_stan$pcv1_bmi[i] <- blood_child_who_stan$pgv1_bmi[i]
  }
}


### categorize PFAS
blood_child_who_stan$PFOS_Total_quar_num<- factor(blood_child_who_stan$PFOS_Total_quar_num,
                                                       levels = 1:4)
blood_child_who_stan$PFOA_Linear_quar_num<- factor(blood_child_who_stan$PFOA_Linear_quar_num,
                                                        levels = 1:4)
blood_child_who_stan$PFDA_quar_num<- factor(blood_child_who_stan$PFDA_quar_num,
                                                 levels = 1:4)
blood_child_who_stan$PFNA_quar_num<- factor(blood_child_who_stan$PFNA_quar_num,
                                                 levels = 1:4)
blood_child_who_stan$PFHxS_quar_num<- factor(blood_child_who_stan$PFHxS_quar_num,
                                                  levels = 1:4)
blood_child_who_stan$PFHpS_quar_num<- factor(blood_child_who_stan$PFHpS_quar_num,
                                                  levels = 1:4)
blood_child_who_stan$PFHpA_quar_num<- factor(blood_child_who_stan$PFHpA_quar_num,
                                                  levels = 1:4)


########################################## long format
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



##############################################################################################################################
# BMI trajectory with bootstrap: Example - PFNA_quar_num (Remember to change PFAS name)

# Create age variables
blood_child_who_stan_long$age1<- blood_child_who_stan_long$Years_since_birth
blood_child_who_stan_long$age2<- (blood_child_who_stan_long$Years_since_birth)^2
blood_child_who_stan_long$age3<- (blood_child_who_stan_long$Years_since_birth)^3

blood_child_who_stan_long$age3.0.5<- (blood_child_who_stan_long$Years_since_birth-0.5)^2*I(blood_child_who_stan_long$Years_since_birth >0.5)


# fit mixed effect model
model<- lme(cbmi ~  age1 + age2 + age3 + age3.0.5,
             random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)

AIC(model)
summary(model)

coef<- model$coefficients$fixed
rand<- model$coefficients$random$Subject_ID2



# age at zenith
age_zenith<- list()

for(i in 1:nrow(rand)){
  age_zenith[[i]]<- uniroot (function(t) {(coef[which(names(coef)=="age1")] + rand[i,2])+ 2*t*coef[which(names(coef)=="age2")] + 3*t^2*coef[which(names(coef)=="age3")] + 3*(t-0.5)^2*I(t-0.5)*coef[which(names(coef)=="age3.0.5")]}, lower=0, upper=1, extendInt = "yes")$root
}

age_zenith<- unlist(age_zenith)



# BMI at zenith
bmi_zenith<- list()

for(i in 1:nrow(rand)){
  bmi_zenith[[i]]<- (coef[which(names(coef)=="(Intercept)")] + rand[i,1]) + (coef[which(names(coef)=="age1")] + rand[i,2]) * age_zenith[i] + 
    coef[which(names(coef)=="age2")]*age_zenith[i]^2 + coef[which(names(coef)=="age3")]*age_zenith[i]^3 + coef[which(names(coef)=="age3.0.5")]*(age_zenith[i]-0.5)^3*I(age_zenith[i] > 0.5)
}

bmi_zenith<- unlist(bmi_zenith)
names(bmi_zenith)<- NULL


blood_child_who_stan<- data.frame(blood_child_who_stan,
                                  bmi_zenith = bmi_zenith,
                                  age_zenith = age_zenith)

##############################################################################################################################
# Linear regression: age at zenith, bmi at zenith


PFHxS_age_zenith<- lm(age_zenith ~ Analysis_Batch_recat + age_at_recruitment +
                        pcv1_highest_education_completed_recat + ethnicity_specified_recat +
                        pcv1_parity_recat + pcv1_smoker + pcv1_bmi +
                        PFHxS_quar_num, data = blood_child_who_stan)

summary(PFHxS_age_zenith)




PFHxS_bmi_zenith<- lm(bmi_zenith ~ Analysis_Batch_recat + age_at_recruitment +
                      pcv1_highest_education_completed_recat + ethnicity_specified_recat +
                      pcv1_parity_recat + pcv1_smoker + pcv1_bmi +
                      PFHxS_quar_num, data = blood_child_who_stan)

summary(PFHxS_bmi_zenith)

