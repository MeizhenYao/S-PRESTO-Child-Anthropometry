###############################################
###############################################
### generating child growth standard scores ###
###############################################
###############################################

library(data.table)  
library(tidyverse)
library(readxl)
library(readr)
library(mice)
library(anthro)
library(writexl)
library(ggh4x)
library(naniar)
library(UpSetR)

#-------------------------------------------------------import data set
whole_cohort<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/whole_cohort_20230316.csv")
child_new<-  read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/Singapore/20220705-anthropometry/FormA43f_Standardized_20231205.xlsx", sheet = "Data")

colnames(child_new)[1]<- "Subject_ID"
#-------------------------------------------------------data preparation


## Restricted to participants with a blood sample and a live birth
blood<- whole_cohort[,c("Subject_ID", "participation", "live_birth_recat", "bw_sex", "bw_birth_GA", 'age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat','pcv1_bmi')] %>% 
  left_join(child_new, by = "Subject_ID") %>% 
  filter(participation==1 & live_birth_recat==1)

## remove some outliers
blood$SP_CWH19WGT<- ifelse(blood$Subject_ID==10322, NA, blood$SP_CWH19WGT)
blood$SP_CWH19WGT<- ifelse(blood$Subject_ID==10406, NA, blood$SP_CWH19WGT)
blood$SP_CWH19WGT<- ifelse(blood$Subject_ID==10731, NA, blood$SP_CWH19WGT)
blood$SP_CWH19WGT<- ifelse(blood$Subject_ID==10442, NA, blood$SP_CWH19WGT)
blood$SP_CWH19LEN<- ifelse(blood$Subject_ID==10573, NA, blood$SP_CWH19LEN)


blood$SP_CWH11WGTRC<- ifelse(blood$SP_CWH11AGE == -1, NA, blood$SP_CWH11WGTRC)
blood$SP_CWH11LENRC<- ifelse(blood$SP_CWH11AGE == -1, NA, blood$SP_CWH11LENRC)
blood$SP_CWH11AGE<- ifelse(blood$SP_CWH11AGE == -1, NA, blood$SP_CWH11AGE)

## merge variables at delivery
blood<- blood %>%
  mutate(SP_CWH11WGT_merge = coalesce(SP_CWH11WGTRC, SP_CWH11WGT),
         SP_CWH11LEN_merge = coalesce(SP_CWH11LENRC, SP_CWH11LEN),
         SP_CWH11AGE_merge = if_else(is.na(SP_CWH11WGTRC) == TRUE, 0, SP_CWH11AGE))


merge<- blood[, c('SP_CWH11WGT_merge', 'SP_CWH11LEN_merge', 'SP_CWH11AGE_merge')]




## only include child anthropometry variables
weight<- c('SP_CWH11WGT_merge','SP_CWH14WGT','SP_CWH15WGT','SP_CWH16WGT','SP_CWH17WGT','SP_CWH19WGT','SP_CWH20WGT', 'SP_CWH21WGT', 'SP_CWH22WGT','SP_CWH23WGT')
length<- c('SP_CWH11LEN_merge','SP_CWH14LEN','SP_CWH15LEN','SP_CWH16LEN','SP_CWH17LEN','SP_CWH19LEN','SP_CWH20LEN', 'SP_CWH21LEN', 'SP_CWH21HGT','SP_CWH22HGT','SP_CWH23HGT')
child_age<- c('SP_CWH11AGE_merge','SP_CWH14AGE','SP_CWH15AGE','SP_CWH16AGE','SP_CWH17AGE','SP_CWH19AGE','SP_CWH20AGE', 'SP_CWH21AGE', 'SP_CWH22AGE','SP_CWH23AGE')


blood_child<- blood[,c("Subject_ID", "bw_sex", "bw_birth_GA", weight, length, child_age,'age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat','pcv1_bmi')]

vis_miss(blood_child)
vis_miss(blood_child,sort_miss = TRUE)
gg_miss_var(blood_child)


blood_child_complete<- blood_child[complete.cases(blood_child),]
# n=96

blood_child_del_m18_whole<- blood_child[complete.cases(blood_child[,c('SP_CWH11WGT_merge', "SP_CWH11LEN_merge", "SP_CWH11AGE_merge", "SP_CWH23WGT", "SP_CWH23HGT", "SP_CWH23AGE")]),]
#n=210




#-------------------------------------------------------choose n=210 as our dataset


##------------------------------------------------------- imputation: for time-dependent covariates
###------------------------------------------------------------------------------ use: time-variable at all timepoints, auxiliary variables

#### to check formats
str(blood_child_del_m18_whole)

blood_child_del_m18_whole$pcv1_highest_education_completed_recat<- as.factor(blood_child_del_m18_whole$pcv1_highest_education_completed_recat)
blood_child_del_m18_whole$ethnicity_specified_recat<- as.factor(blood_child_del_m18_whole$ethnicity_specified_recat)
blood_child_del_m18_whole$pcv1_parity_recat<- as.factor(blood_child_del_m18_whole$pcv1_parity_recat)

str(blood_child_del_m18_whole)

#### to check missingness
blood_child_del_m18<- blood_child_del_m18_whole %>% 
                      dplyr::select(-all_of(weight), -all_of(length))

blood_child_del_m18_for_impute<- subset(blood_child_del_m18, select = -(Subject_ID))
apply(blood_child_del_m18_for_impute, 2, function(x) sum(is.na(x))) 
md.pattern(blood_child_del_m18_for_impute)


vis_miss(blood_child_del_m18_for_impute)
gg_miss_var(blood_child_del_m18_for_impute)

#### Initialize the Imputation
init = mice(blood_child_del_m18_for_impute, maxit=0) 
meth = init$method
predM = init$predictorMatrix



####  Specify a method to be used for imputation for some selected variables 
#####   used Random Forest(rf) for selected variables, and blank for other variables
meth[c('age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat', "bw_sex", "bw_birth_GA")] = ""

meth[c(child_age, "pcv1_bmi")] = "rf"


####   Now Run your Main Imputation using "Random Forest"
#####   m = 5 creates 5 different imputed datasets 
set.seed(951263)
blood_child_del_m18_imputing = mice(blood_child_del_m18_for_impute, method=meth, predictorMatrix=predM, m=8, ntree = 50)
#####    Use the last completed data set 
blood_child_imputed <- complete(blood_child_del_m18_imputing, action=8)


####    Check if you actually imputed all the variables you wanted to impute
sapply(blood_child_imputed, function(x) sum(is.na(x)))


blood_child_imputed_age<- blood_child_imputed 



##------------------------------------------------------- merge with original time-dependent outcome variables
blood_child_imputed_mix<- data.frame(blood_child_del_m18_whole[, c("Subject_ID", weight, length)],
                                     blood_child_imputed_age) 






#------------------------------------------------------- WHO child growth standards

#----------------- complete cases data

## delivery (merge hospital records & delivery visit)
blood_child_who_stan_del_merge<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH11AGE_merge, 
                 weight = SP_CWH11WGT_merge/1000, 
                 lenhei = SP_CWH11LEN_merge,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")] 

names(blood_child_who_stan_del_merge)<- c("zweight_del_merge", "zlength_del_merge", "zbmi_del_merge", "zwfl_WHO_del_merge", "cbmi_del_merge")


## at wk3
blood_child_who_stan_wk3<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH14AGE, 
                 weight = SP_CWH14WGT/1000, 
                 lenhei = SP_CWH14LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_wk3)<- c("zweight_wk3", "zlength_wk3", "zbmi_wk3", "zwfl_WHO_wk3", "cbmi_wk3")

## at wk6
blood_child_who_stan_wk6<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH15AGE, 
                 weight = SP_CWH15WGT/1000, 
                 lenhei = SP_CWH15LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_wk6)<- c("zweight_wk6", "zlength_wk6", "zbmi_wk6", "zwfl_WHO_wk6", "cbmi_wk6")

## at m3
blood_child_who_stan_m3<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH16AGE, 
                 weight = SP_CWH16WGT/1000, 
                 lenhei = SP_CWH16LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_m3)<- c("zweight_m3", "zlength_m3", "zbmi_m3", "zwfl_WHO_m3", "cbmi_m3")

## at m6
blood_child_who_stan_m6<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH17AGE, 
                 weight = SP_CWH17WGT/1000, 
                 lenhei = SP_CWH17LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_m6)<- c("zweight_m6", "zlength_m6", "zbmi_m6", "zwfl_WHO_m6", "cbmi_m6")

## at m12
blood_child_who_stan_m12<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH19AGE, 
                 weight = SP_CWH19WGT/1000, 
                 lenhei = SP_CWH19LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]


names(blood_child_who_stan_m12)<- c("zweight_m12", "zlength_m12", "zbmi_m12", "zwfl_WHO_m12", "cbmi_m12")

## at m18
blood_child_who_stan_m18<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH20AGE, 
                 weight = SP_CWH20WGT/1000, 
                 lenhei = SP_CWH20LEN,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_m18)<- c("zweight_m18", "zlength_m18", "zbmi_m18", "zwfl_WHO_m18", "cbmi_m18")


## at m24
blood_child_who_stan_m24<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH21AGE, 
                 weight = SP_CWH21WGT/1000, 
                 lenhei = SP_CWH21HGT,
                 measure = "H")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_m24)<- c("zweight_m24", "zlength_m24", "zbmi_m24", "zwfl_WHO_m24", "cbmi_m24")

## at m36
blood_child_who_stan_m36<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH22AGE, 
                 weight = SP_CWH22WGT, 
                 lenhei = SP_CWH22HGT,
                 measure = "H")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_m36)<- c("zweight_m36", "zlength_m36", "zbmi_m36", "zwfl_WHO_m36", "cbmi_m36")


## at Y4
blood_child_who_stan_y4<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = SP_CWH23AGE, 
                 weight = SP_CWH23WGT, 
                 lenhei = SP_CWH23HGT,
                 measure = "H")
)[,c("zwei", "zlen", "zbmi", "zwfl", "cbmi")]

names(blood_child_who_stan_y4)<- c("zweight_y4", "zlength_y4", "zbmi_y4", "zwfl_WHO_y4", "cbmi_y4")




# Internal z-scores for weight-to-height ratio
wfl_internal_zscores_female<- blood_child_imputed_mix %>% 
  filter(bw_sex == 2) %>% 
  mutate(zwfl_internal_del_merge = (SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge) - mean(na.rm = TRUE,SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge)))/sd(na.rm = TRUE,SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge)),
         zwfl_internal_wk3 = (SP_CWH14WGT/(1000*SP_CWH14LEN) - mean(na.rm = TRUE,SP_CWH14WGT/(1000*SP_CWH14LEN)))/sd(na.rm = TRUE,SP_CWH14WGT/(1000*SP_CWH14LEN)),
         zwfl_internal_wk6 = (SP_CWH15WGT/(1000*SP_CWH15LEN) - mean(na.rm = TRUE,SP_CWH15WGT/(1000*SP_CWH15LEN)))/sd(na.rm = TRUE,SP_CWH15WGT/(1000*SP_CWH15LEN)),
         zwfl_internal_m3 = (SP_CWH16WGT/(1000*SP_CWH16LEN) - mean(na.rm = TRUE,SP_CWH16WGT/(1000*SP_CWH16LEN)))/sd(na.rm = TRUE,SP_CWH16WGT/(1000*SP_CWH16LEN)),
         zwfl_internal_m6 = (SP_CWH17WGT/(1000*SP_CWH17LEN) - mean(na.rm = TRUE,SP_CWH17WGT/(1000*SP_CWH17LEN)))/sd(na.rm = TRUE,SP_CWH17WGT/(1000*SP_CWH17LEN)),
         zwfl_internal_m12 = (SP_CWH19WGT/(1000*SP_CWH19LEN) - mean(na.rm = TRUE,SP_CWH19WGT/(1000*SP_CWH19LEN)))/sd(na.rm = TRUE,SP_CWH19WGT/(1000*SP_CWH19LEN)),
         zwfl_internal_m18 = (SP_CWH20WGT/(1000*SP_CWH20LEN) - mean(na.rm = TRUE,SP_CWH20WGT/(1000*SP_CWH20LEN)))/sd(na.rm = TRUE,SP_CWH20WGT/(1000*SP_CWH20LEN)),
         zwfl_internal_m24 = (SP_CWH21WGT/(1000*SP_CWH21HGT) - mean(na.rm = TRUE,SP_CWH21WGT/(1000*SP_CWH21HGT)))/sd(na.rm = TRUE,SP_CWH21WGT/(1000*SP_CWH21HGT)),
         zwfl_internal_m36 = (SP_CWH22WGT/(SP_CWH22HGT) - mean(na.rm = TRUE,SP_CWH22WGT/(SP_CWH22HGT)))/sd(na.rm = TRUE,SP_CWH22WGT/(SP_CWH22HGT)),
         zwfl_internal_y4 = (SP_CWH23WGT/(SP_CWH23HGT) - mean(na.rm = TRUE,SP_CWH23WGT/(SP_CWH23HGT)))/sd(na.rm = TRUE,SP_CWH23WGT/(SP_CWH23HGT))) %>% 
  dplyr::select(Subject_ID, zwfl_internal_del_merge, zwfl_internal_wk3, zwfl_internal_wk6, zwfl_internal_m3, zwfl_internal_m6, zwfl_internal_m12, zwfl_internal_m18, zwfl_internal_m24, zwfl_internal_m36, zwfl_internal_y4)

wfl_internal_zscores_male<- blood_child_imputed_mix %>% 
  filter(bw_sex == 1) %>% 
  mutate(zwfl_internal_del_merge = (SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge) - mean(na.rm = TRUE,SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge)))/sd(na.rm = TRUE,SP_CWH11WGT_merge/(1000*SP_CWH11LEN_merge)),
         zwfl_internal_wk3 = (SP_CWH14WGT/(1000*SP_CWH14LEN) - mean(na.rm = TRUE,SP_CWH14WGT/(1000*SP_CWH14LEN)))/sd(na.rm = TRUE,SP_CWH14WGT/(1000*SP_CWH14LEN)),
         zwfl_internal_wk6 = (SP_CWH15WGT/(1000*SP_CWH15LEN) - mean(na.rm = TRUE,SP_CWH15WGT/(1000*SP_CWH15LEN)))/sd(na.rm = TRUE,SP_CWH15WGT/(1000*SP_CWH15LEN)),
         zwfl_internal_m3 = (SP_CWH16WGT/(1000*SP_CWH16LEN) - mean(na.rm = TRUE,SP_CWH16WGT/(1000*SP_CWH16LEN)))/sd(na.rm = TRUE,SP_CWH16WGT/(1000*SP_CWH16LEN)),
         zwfl_internal_m6 = (SP_CWH17WGT/(1000*SP_CWH17LEN) - mean(na.rm = TRUE,SP_CWH17WGT/(1000*SP_CWH17LEN)))/sd(na.rm = TRUE,SP_CWH17WGT/(1000*SP_CWH17LEN)),
         zwfl_internal_m12 = (SP_CWH19WGT/(1000*SP_CWH19LEN) - mean(na.rm = TRUE,SP_CWH19WGT/(1000*SP_CWH19LEN)))/sd(na.rm = TRUE,SP_CWH19WGT/(1000*SP_CWH19LEN)),
         zwfl_internal_m18 = (SP_CWH20WGT/(1000*SP_CWH20LEN) - mean(na.rm = TRUE,SP_CWH20WGT/(1000*SP_CWH20LEN)))/sd(na.rm = TRUE,SP_CWH20WGT/(1000*SP_CWH20LEN)),
         zwfl_internal_m24 = (SP_CWH21WGT/(1000*SP_CWH21HGT) - mean(na.rm = TRUE,SP_CWH21WGT/(1000*SP_CWH21HGT)))/sd(na.rm = TRUE,SP_CWH21WGT/(1000*SP_CWH21HGT)),
         zwfl_internal_m36 = (SP_CWH22WGT/(SP_CWH22HGT) - mean(na.rm = TRUE,SP_CWH22WGT/(SP_CWH22HGT)))/sd(na.rm = TRUE,SP_CWH22WGT/(SP_CWH22HGT)),
         zwfl_internal_y4 = (SP_CWH23WGT/(SP_CWH23HGT) - mean(na.rm = TRUE,SP_CWH23WGT/(SP_CWH23HGT)))/sd(na.rm = TRUE,SP_CWH23WGT/(SP_CWH23HGT))) %>% 
  dplyr::select(Subject_ID, zwfl_internal_del_merge, zwfl_internal_wk3, zwfl_internal_wk6, zwfl_internal_m3, zwfl_internal_m6, zwfl_internal_m12, zwfl_internal_m18, zwfl_internal_m24, zwfl_internal_m36, zwfl_internal_y4)

wfl_internal_zscores<- rbind(wfl_internal_zscores_female, wfl_internal_zscores_male)


## combine all datasets and add 
blood_child_who_stan_impute_mix<- data.frame(blood_child_imputed_mix[,c("Subject_ID", weight, length, child_age, "bw_sex", "bw_birth_GA")],
                                             blood_child_who_stan_del_merge,
                                             blood_child_who_stan_wk3,
                                             blood_child_who_stan_wk6,
                                             blood_child_who_stan_m3,
                                             blood_child_who_stan_m6,
                                             blood_child_who_stan_m12,
                                             blood_child_who_stan_m18,
                                             blood_child_who_stan_m24,
                                             blood_child_who_stan_m36,
                                             blood_child_who_stan_y4) %>% 
  inner_join(wfl_internal_zscores, by="Subject_ID")


write_csv(blood_child_who_stan_impute_mix, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_impute_toy4_12182023.csv")









