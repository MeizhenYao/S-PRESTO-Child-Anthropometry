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

##########################################
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
# BMI trajectory with bootstrap: Example - PFNA_quar_num (Remember to change PFAS name)

### Create age variables
blood_child_who_stan_long$age1<- blood_child_who_stan_long$Years_since_birth
blood_child_who_stan_long$age2<- (blood_child_who_stan_long$Years_since_birth)^2
blood_child_who_stan_long$age3<- (blood_child_who_stan_long$Years_since_birth)^3

blood_child_who_stan_long$age3.0.5<- (blood_child_who_stan_long$Years_since_birth-0.5)^3*I(blood_child_who_stan_long$Years_since_birth >0.5)


# Define the Parametric Bootstrap Function:

parametric_bootstrap <- function(data, indices) {
  boot_sample <- data[indices, ]
  model <- lme(cbmi ~  Analysis_Batch_recat + age_at_recruitment +
                 pcv1_highest_education_completed_recat + ethnicity_specified_recat +
                 pcv1_parity_recat + pcv1_smoker + pcv1_bmi +
                 PFNA_quar_num*(age1 + age2+ age3 +age3.0.5),
               random = ~1 + age1|Subject_ID2, data = boot_sample, na.action=na.omit,control = lmeControl(opt = "optim", optCtrl = list(method = "L-BFGS-B")))
  
  coef<- model$coefficients$fixed
  return(coef)  
}


data = blood_child_who_stan_long


# Parallelize Bootstrap Sampling: 
cores <- detectCores() 
cl <- makeCluster(cores)
clusterExport(cl, c("data", "parametric_bootstrap"))

B <- 100  # Number of bootstrap iterations
results <- mclapply(1:B, function(i) {
  set.seed(i)
  indices <- sample(nrow(data), replace = TRUE) # change PFAS name
  parametric_bootstrap(data, indices)
}, mc.cores = 1)

stopCluster(cl)


# Combine the results into a matrix and calculate interested quantity:
results_matrix <- data.frame(do.call(rbind, results))

############################################################################################################
## 1. age at zenith

### at PFAS level 1: 
results_matrix$PFAS_level1_zenith_age<- rep(NA, B)
for(i in 1:nrow(results_matrix)){
  
  results_matrix$PFAS_level1_zenith_age[i]<- uniroot(function(t) {results_matrix$age1[i] + 2*t*results_matrix$age2[i] + 3*t^2*results_matrix$age3[i] + 3*(t-0.5)^2*I(t>0.5)*results_matrix$age3.0.5[i]} , interval=c(0, 1))
  
}
results_matrix$PFAS_level1_zenith_age<- unlist(results_matrix$PFAS_level1_zenith_age)


### at PFAS level 2: 
results_matrix$PFAS_level2_zenith_age<- rep(NA, B)
for(i in 1:nrow(results_matrix)){
  
  results_matrix$PFAS_level2_zenith_age[i]<- uniroot(function(t) {(results_matrix$age1[i] + results_matrix$PFNA_quar_num2.age1[i]) + 
      2*t*(results_matrix$age2[i]+ results_matrix$PFNA_quar_num2.age2[i]) + 3*t^2*(results_matrix$age3[i]+ results_matrix$PFNA_quar_num2.age3[i]) + 
      3*(t-0.5)^2*I(t>0.5)*(results_matrix$age3.0.5[i] + results_matrix$PFNA_quar_num2.age3.0.5[i])} , interval=c(0, 1.2))
  
}
results_matrix$PFAS_level2_zenith_age<- unlist(results_matrix$PFAS_level2_zenith_age)


### at PFAS level 3: 
results_matrix$PFAS_level3_zenith_age<- rep(NA, B)
for(i in 1:nrow(results_matrix)){
  
  results_matrix$PFAS_level3_zenith_age[i]<- uniroot(function(t) {(results_matrix$age1[i] + results_matrix$PFNA_quar_num3.age1[i]) + 
      2*t*(results_matrix$age2[i]+ results_matrix$PFNA_quar_num3.age2[i]) + 3*t^2*(results_matrix$age3[i]+ results_matrix$PFNA_quar_num3.age3[i]) + 
      3*(t-0.5)^2*I(t>0.5)*(results_matrix$age3.0.5[i] + results_matrix$PFNA_quar_num3.age3.0.5[i])} , interval=c(0, 1))
  
}
results_matrix$PFAS_level3_zenith_age<- unlist(results_matrix$PFAS_level3_zenith_age)


### at PFAS level 4: 
results_matrix$PFAS_level4_zenith_age<- rep(NA, B)
for(i in 1:nrow(results_matrix)){
  
  results_matrix$PFAS_level4_zenith_age[i]<- uniroot(function(t) {(results_matrix$age1[i] + results_matrix$PFNA_quar_num4.age1[i]) + 
      2*t*(results_matrix$age2[i]+ results_matrix$PFNA_quar_num4.age2[i]) + 3*t^2*(results_matrix$age3[i]+ results_matrix$PFNA_quar_num4.age3[i]) + 
      3*(t-0.5)^2*I(t>0.5)*(results_matrix$age3.0.5[i] + results_matrix$PFNA_quar_num4.age3.0.5[i])} , interval=c(0, 1.2))
  
}
results_matrix$PFAS_level4_zenith_age<- unlist(results_matrix$PFAS_level4_zenith_age)

### difference: level 2 vs. level 1
results_matrix$PFAS_level21_zenith_age<- results_matrix$PFAS_level2_zenith_age - results_matrix$PFAS_level1_zenith_age


### difference: level 3 vs. level 1
results_matrix$PFAS_level31_zenith_age<- results_matrix$PFAS_level3_zenith_age - results_matrix$PFAS_level1_zenith_age

### difference: level 4 vs. level 1
results_matrix$PFAS_level41_zenith_age<- results_matrix$PFAS_level4_zenith_age - results_matrix$PFAS_level1_zenith_age


## Calculate confidence intervals using the percentile method:
### mean: at PFAS level 1: 
PFAS_level1_zenith_meanage<- mean(results_matrix$PFAS_level1_zenith_age)
PFAS_level1_zenith_meanage_ci <- quantile(results_matrix$PFAS_level1_zenith_age, c(0.025, 0.975))
PFAS_level1_zenith_age<- c("age at zenith, PFAS level 1", PFAS_level1_zenith_meanage, PFAS_level1_zenith_meanage_ci)

### mean: at PFAS level 2: 
PFAS_level2_zenith_meanage<- mean(results_matrix$PFAS_level2_zenith_age)
PFAS_level2_zenith_meanage_ci <- quantile(results_matrix$PFAS_level2_zenith_age, c(0.025, 0.975))
PFAS_level2_zenith_age<- c("age at zenith, PFAS level 2", PFAS_level2_zenith_meanage, PFAS_level2_zenith_meanage_ci)

### mean: at PFAS level 3: 
PFAS_level3_zenith_meanage<- mean(results_matrix$PFAS_level3_zenith_age)
PFAS_level3_zenith_meanage_ci <- quantile(results_matrix$PFAS_level3_zenith_age, c(0.025, 0.975))
PFAS_level3_zenith_age<- c("age at zenith, PFAS level 3", PFAS_level3_zenith_meanage, PFAS_level3_zenith_meanage_ci)

### mean: at PFAS level 4: 
PFAS_level4_zenith_meanage<- mean(results_matrix$PFAS_level4_zenith_age)
PFAS_level4_zenith_meanage_ci <- quantile(results_matrix$PFAS_level4_zenith_age, c(0.025, 0.975))
PFAS_level4_zenith_age<- c("age at zenith, PFAS level 4", PFAS_level4_zenith_meanage, PFAS_level4_zenith_meanage_ci)

### difference: level 2 vs. level 1
PFAS_level21_zenith_meanage<- mean(results_matrix$PFAS_level21_zenith_age)
PFAS_level21_zenith_meanage_ci <- quantile(results_matrix$PFAS_level21_zenith_age, c(0.025, 0.975))
PFAS_level21_zenith_age<- c("age at zenith, PFAS level 2 vs. 1", PFAS_level21_zenith_meanage, PFAS_level21_zenith_meanage_ci)

### difference: level 3 vs. level 1
PFAS_level31_zenith_meanage<- mean(results_matrix$PFAS_level31_zenith_age)
PFAS_level31_zenith_meanage_ci <- quantile(results_matrix$PFAS_level31_zenith_age, c(0.025, 0.975))
PFAS_level31_zenith_age<- c("age at zenith, PFAS level 3 vs. 1", PFAS_level31_zenith_meanage, PFAS_level31_zenith_meanage_ci)

### difference: level 4 vs. level 1
PFAS_level41_zenith_meanage<- mean(results_matrix$PFAS_level41_zenith_age)
PFAS_level41_zenith_meanage_ci <- quantile(results_matrix$PFAS_level41_zenith_age, c(0.025, 0.975))
PFAS_level41_zenith_age<- c("age at zenith, PFAS level 4 vs. 1", PFAS_level41_zenith_meanage, PFAS_level41_zenith_meanage_ci)

PFAS_zenith_age<- data.frame(rbind(PFAS_level1_zenith_age,
                             PFAS_level2_zenith_age,
                             PFAS_level3_zenith_age,
                             PFAS_level4_zenith_age,
                             PFAS_level21_zenith_age,
                             PFAS_level31_zenith_age,
                             PFAS_level41_zenith_age
                             ))
names(PFAS_zenith_age)[1]<- "level"
names(PFAS_zenith_age)[2]<- "mean"
names(PFAS_zenith_age)[3]<- "lower ci"
names(PFAS_zenith_age)[4]<- "higher ci"


############################################################################################################
## 2. bmi at zenith

## linear mixed effect model we used in the bootstrap process
model<- lme(cbmi ~  Analysis_Batch_recat + age_at_recruitment +
              pcv1_highest_education_completed_recat + ethnicity_specified_recat +
              pcv1_parity_recat + pcv1_smoker + pcv1_bmi +
              PFNA_quar_num*(age1 + age2+ age3 +age3.0.5),
            random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)


## remember to change PFAS name below
data_pred<- list()
age1<- list()
age2<- list()
age3<- list()
age3.0.5<- list()
bmi_results<- list()
bmi_zenith<- list()

for(i in 1:nrow(results_matrix)){
age1[[i]]<- c(rep(results_matrix$PFAS_level1_zenith_age[i], table(blood_child_who_stan_long$PFNA_quar_num)[1]/10), rep(results_matrix$PFAS_level2_zenith_age[i], table(blood_child_who_stan_long$PFNA_quar_num)[2]/10),
         rep(results_matrix$PFAS_level3_zenith_age[i], table(blood_child_who_stan_long$PFNA_quar_num)[3]/10), rep(results_matrix$PFAS_level4_zenith_age[i], table(blood_child_who_stan_long$PFNA_quar_num)[4]/10))
age2[[i]] <- age1[[i]]^2 
age3[[i]] <- age1[[i]]^3 
age3.0.5[[i]]<-(age1[[i]]-0.5)^3*I(age1[[i]]>0.5) 

covariates<- blood_child_who_stan_long %>% 
  filter(visit == "m6") %>% 
  arrange(by = PFNA_quar_num) %>% 
  select(Subject_ID2, PFNA_quar_num, Analysis_Batch_recat, age_at_recruitment, pcv1_highest_education_completed_recat,
         ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker, pcv1_bmi)


data_pred[[i]]<- data.frame(age1 = age1[[i]], 
                            age2 = age2[[i]], 
                            age3 = age3[[i]], 
                            age3.0.5 = age3.0.5[[i]], covariates) 

bmi_results[[i]]<- predict(model, newdata= data_pred[[i]], level=0) 
bmi_results[[i]]<- data.frame(data_pred[[i]],
                              bmi = bmi_results[[i]])


bmi_zenith[[i]]<- c(mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==1,]$bmi), 
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==2,]$bmi),
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==3,]$bmi),
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==4,]$bmi),
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==2,]$bmi) - mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==1,]$bmi),
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==3,]$bmi) - mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==1,]$bmi),
                    mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==4,]$bmi) - mean(bmi_results[[i]][bmi_results[[i]]$PFNA_quar_num==1,]$bmi)
                    )

}



bmi_zenith_results <- data.frame(do.call(rbind, bmi_zenith))
names(bmi_zenith_results)[1]<- "PFAS_level1_zenith_bmi"
names(bmi_zenith_results)[2]<- "PFAS_level2_zenith_bmi"
names(bmi_zenith_results)[3]<- "PFAS_level3_zenith_bmi"
names(bmi_zenith_results)[4]<- "PFAS_level4_zenith_bmi"
names(bmi_zenith_results)[5]<- "PFAS_level21_zenith_bmi"
names(bmi_zenith_results)[6]<- "PFAS_level31_zenith_bmi"
names(bmi_zenith_results)[7]<- "PFAS_level41_zenith_bmi"



### mean: at PFAS level 1: 
PFAS_level1_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level1_zenith_bmi)
PFAS_level1_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level1_zenith_bmi, c(0.025, 0.975))
PFAS_level1_zenith_bmi<- c("bmi at zenith, PFAS level 1", PFAS_level1_zenith_meanbmi, PFAS_level1_zenith_meanbmi_ci)

### mean: at PFAS level 2: 
PFAS_level2_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level2_zenith_bmi)
PFAS_level2_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level2_zenith_bmi, c(0.025, 0.975))
PFAS_level2_zenith_bmi<- c("bmi at zenith, PFAS level 2", PFAS_level2_zenith_meanbmi, PFAS_level2_zenith_meanbmi_ci)

### mean: at PFAS level 3: 
PFAS_level3_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level3_zenith_bmi)
PFAS_level3_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level3_zenith_bmi, c(0.025, 0.975))
PFAS_level3_zenith_bmi<- c("bmi at zenith, PFAS level 3", PFAS_level3_zenith_meanbmi, PFAS_level3_zenith_meanbmi_ci)

### mean: at PFAS level 4: 
PFAS_level4_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level4_zenith_bmi)
PFAS_level4_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level4_zenith_bmi, c(0.025, 0.975))
PFAS_level4_zenith_bmi<- c("bmi at zenith, PFAS level 4", PFAS_level4_zenith_meanbmi, PFAS_level4_zenith_meanbmi_ci)



### difference: level 2 vs. level 1
PFAS_level21_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level21_zenith_bmi)
PFAS_level21_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level21_zenith_bmi, c(0.025, 0.975))
PFAS_level21_zenith_bmi<- c("bmi at zenith, PFAS level 2 vs. 1", PFAS_level21_zenith_meanbmi, PFAS_level21_zenith_meanbmi_ci)

### difference: level 3 vs. level 1
PFAS_level31_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level31_zenith_bmi)
PFAS_level31_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level31_zenith_bmi, c(0.025, 0.975))
PFAS_level31_zenith_bmi<- c("bmi at zenith, PFAS level 3 vs. 1", PFAS_level31_zenith_meanbmi, PFAS_level31_zenith_meanbmi_ci)

### difference: level 4 vs. level 1
PFAS_level41_zenith_meanbmi<- mean(bmi_zenith_results$PFAS_level41_zenith_bmi)
PFAS_level41_zenith_meanbmi_ci <- quantile(bmi_zenith_results$PFAS_level41_zenith_bmi, c(0.025, 0.975))
PFAS_level41_zenith_bmi<- c("bmi at zenith, PFAS level 4 vs. 1", PFAS_level41_zenith_meanbmi, PFAS_level41_zenith_meanbmi_ci)

PFAS_zenith_bmi<- data.frame(rbind(PFAS_level1_zenith_bmi,
                        PFAS_level2_zenith_bmi,
                        PFAS_level3_zenith_bmi,
                        PFAS_level4_zenith_bmi,
                        PFAS_level21_zenith_bmi,
                        PFAS_level31_zenith_bmi,
                        PFAS_level41_zenith_bmi
))
names(PFAS_zenith_bmi)[1]<- "level"
names(PFAS_zenith_bmi)[2]<- "mean"
names(PFAS_zenith_bmi)[3]<- "lower ci"
names(PFAS_zenith_bmi)[4]<- "higher ci"

PFAS_zenith<- rbind(PFAS_zenith_age,
                    PFAS_zenith_bmi)


write.table(PFAS_zenith, "/sc/arion/projects/Faroese/pfas_met/c18/pfas_22_metabolites_all_c18.txt", row.names = F)

############################################################################################################
## Global p-value for interaction term

#### without interaction
model1<- lme(cbmi ~  Analysis_Batch_recat + age_at_recruitment +
               pcv1_highest_education_completed_recat + ethnicity_specified_recat +
               pcv1_parity_recat + pcv1_smoker + pcv1_bmi + PFNA_quar_num +
               age1 + age2+ age3 +age3.0.5,
             random = ~1 + age1|Subject_ID2, data = blood_child_who_stan_long, na.action=na.omit)


anova(model, model1)











