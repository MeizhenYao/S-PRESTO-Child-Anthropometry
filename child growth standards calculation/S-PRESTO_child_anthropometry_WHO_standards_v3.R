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
fetal_growth<-  read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/Singapore/20231113_fetal_growth/ultrasound_CRF_20220218.xlsx", sheet = "Data")


#-------------------------------------------------------data preparation


## Restricted to participants with a blood sample and a live birth
blood<- whole_cohort %>% 
  filter(participation==1 & live_birth_recat==1)

## remove some outliers
blood$weight_m12<- ifelse(blood$Subject_ID==10322, NA, blood$weight_m12)
blood$weight_m12<- ifelse(blood$Subject_ID==10406, NA, blood$weight_m12)
blood$weight_m12<- ifelse(blood$Subject_ID==10731, NA, blood$weight_m12)
blood$weight_m12<- ifelse(blood$Subject_ID==10442, NA, blood$weight_m12)
blood$length_m12<- ifelse(blood$Subject_ID==10573, NA, blood$length_m12)

## add days_since_birth_del_CN variable
blood$Days_since_birth_del_CN<- rep(0, 328)


blood$Days_since_birth_del<- ifelse(blood$Days_since_birth_del == -1, NA, blood$Days_since_birth_del)
blood$weight_del<- ifelse(blood$Days_since_birth_del == -1, NA, blood$weight_del)
blood$length_del<- ifelse(blood$Days_since_birth_del == -1, NA, blood$length_del)

## merge variables at delivery
del<- blood[, c('weight_del', 'length_del', 'Days_since_birth_del')]

blood<- blood %>%
  mutate(weight_del_merge = coalesce(weight_del, weight_del_CN),
         length_del_merge = coalesce(length_del, length_del_CN),
         Days_since_birth_del_merge = if_else(is.na(weight_del) == TRUE, 0, Days_since_birth_del))


merge<- blood[, c('weight_del_merge', 'length_del_merge', 'Days_since_birth_del_merge')]




## only include child anthropometry variables
weight<- c('weight_del_merge','weight_wk1','weight_wk3','weight_wk6','weight_m3','weight_m6','weight_m12','weight_m18')
length<- c('length_del_merge',"length_wk1",'length_wk3','length_wk6','length_m3','length_m6','length_m12','length_m18')
child_age<- c('Days_since_birth_del_merge',"Days_since_birth_wk1",'Days_since_birth_wk3','Days_since_birth_wk6','Days_since_birth_m3','Days_since_birth_m6','Days_since_birth_m12','Days_since_birth_m18')


blood_child<- blood[,c("Subject_ID", "bw_sex", "bw_birth_GA", weight, length, child_age,'age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat','pcv1_bmi')]

vis_miss(blood_child)
vis_miss(blood_child,sort_miss = TRUE)
gg_miss_var(blood_child)


blood_child_complete<- blood_child[complete.cases(blood_child),]
# n=187
blood_child_del_m18<- blood_child[complete.cases(blood_child[,c('weight_del_merge', "length_del_merge", "Days_since_birth_del_merge", "weight_m18", "length_m18", "Days_since_birth_m18")]),]
#n=240




#-------------------------------------------------------choose n=240 as our dataset


##------------------------------------------------------- imputation: for time-dependent covariates
###------------------------------------------------------------------------------ use: outcome at all timepoints, time-variable at all timepoints, auxiliary variables

#### to check formats
str(blood_child_del_m18)

blood_child_del_m18$pcv1_highest_education_completed_recat<- as.factor(blood_child_del_m18$pcv1_highest_education_completed_recat)
blood_child_del_m18$ethnicity_specified_recat<- as.factor(blood_child_del_m18$ethnicity_specified_recat)
blood_child_del_m18$pcv1_parity_recat<- as.factor(blood_child_del_m18$pcv1_parity_recat)

str(blood_child_del_m18)

#### to check missingness

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
meth[c('age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat', "bw_sex", "bw_birth_GA", "pcv1_bmi")] = ""

meth[c(weight, length, child_age)] = "rf"


####   Now Run your Main Imputation using "Random Forest"
#####   m = 5 creates 5 different imputed datasets 
set.seed(951263)
blood_child_del_m18_imputing = mice(blood_child_del_m18_for_impute, method=meth, predictorMatrix=predM, m=8, ntree = 50)
#####    Use the last completed data set 
blood_child_imputed <- complete(blood_child_del_m18_imputing, action=8)


####    Check if you actually imputed all the variables you wanted to impute
sapply(blood_child_imputed, function(x) sum(is.na(x)))


blood_child_imputed_age<- blood_child_imputed %>% 
                          select(-all_of(weight), -all_of(length))



##------------------------------------------------------- merge with original time-dependent outcome variables
blood_child_imputed_mix<- data.frame(blood_child_del_m18[, c("Subject_ID", weight, length)],
                                     blood_child_imputed_age) %>% 
                          mutate(measure="L")




#------------------------------------------------------- WHO child growth standards

#----------------- complete cases data

## delivery (merge hospital records & delivery visit)
blood_child_who_stan_del_merge<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_del_merge, 
                 weight = weight_del_merge/1000, 
                 lenhei = length_del_merge,
                 measure = measure)
)[,c("zwei", "zlen", "zbmi", "zwfl")] 

names(blood_child_who_stan_del_merge)<- c("zweight_del_merge", "zlength_del_merge", "zbmi_del_merge", "zwfl_WHO_del_merge")



## at wk3
blood_child_who_stan_wk3<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk3, 
                 weight = weight_wk3/1000, 
                 lenhei = length_wk3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk3)<- c("zweight_wk3", "zlength_wk3", "zbmi_wk3", "zwfl_WHO_wk3")

## at wk6
blood_child_who_stan_wk6<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk6, 
                 weight = weight_wk6/1000, 
                 lenhei = length_wk6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk6)<- c("zweight_wk6", "zlength_wk6", "zbmi_wk6", "zwfl_WHO_wk6")

## at m3
blood_child_who_stan_m3<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m3, 
                 weight = weight_m3/1000, 
                 lenhei = length_m3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m3)<- c("zweight_m3", "zlength_m3", "zbmi_m3", "zwfl_WHO_m3")

## at m6
blood_child_who_stan_m6<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m6, 
                 weight = weight_m6/1000, 
                 lenhei = length_m6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m6)<- c("zweight_m6", "zlength_m6", "zbmi_m6", "zwfl_WHO_m6")

## at m12
blood_child_who_stan_m12<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m12, 
                 weight = weight_m12/1000, 
                 lenhei = length_m12,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]


names(blood_child_who_stan_m12)<- c("zweight_m12", "zlength_m12", "zbmi_m12", "zwfl_WHO_m12")

## at m18
blood_child_who_stan_m18<- with(
  blood_child_imputed_mix,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m18, 
                 weight = weight_m18/1000, 
                 lenhei = length_m18,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m18)<- c("zweight_m18", "zlength_m18", "zbmi_m18", "zwfl_WHO_m18")

# Internal z-scores for weight-to-height ratio
wfl_internal_zscores_female<- blood_child_imputed_mix %>% 
  filter(bw_sex == 2) %>% 
  mutate(zwfl_internal_del_merge = (weight_del_merge/(1000*length_del_merge) - mean(na.rm = TRUE,weight_del_merge/(1000*length_del_merge)))/sd(na.rm = TRUE,weight_del_merge/(1000*length_del_merge)),
         zwfl_internal_wk3 = (weight_wk3/(1000*length_wk3) - mean(na.rm = TRUE,weight_wk3/(1000*length_wk3)))/sd(na.rm = TRUE,weight_wk3/(1000*length_wk3)),
         zwfl_internal_wk6 = (weight_wk6/(1000*length_wk6) - mean(na.rm = TRUE,weight_wk6/(1000*length_wk6)))/sd(na.rm = TRUE,weight_wk6/(1000*length_wk6)),
         zwfl_internal_m3 = (weight_m3/(1000*length_m3) - mean(na.rm = TRUE,weight_m3/(1000*length_m3)))/sd(na.rm = TRUE,weight_m3/(1000*length_m3)),
         zwfl_internal_m6 = (weight_m6/(1000*length_m6) - mean(na.rm = TRUE,weight_m6/(1000*length_m6)))/sd(na.rm = TRUE,weight_m6/(1000*length_m6)),
         zwfl_internal_m12 = (weight_m12/(1000*length_m12) - mean(na.rm = TRUE,weight_m12/(1000*length_m12)))/sd(na.rm = TRUE,weight_m12/(1000*length_m12)),
         zwfl_internal_m18 = (weight_m18/(1000*length_m18) - mean(na.rm = TRUE,weight_m18/(1000*length_m18)))/sd(na.rm = TRUE,weight_m18/(1000*length_m18))) %>% 
  select(Subject_ID, zwfl_internal_del_merge, zwfl_internal_wk3, zwfl_internal_wk6, zwfl_internal_m3, zwfl_internal_m6, zwfl_internal_m12, zwfl_internal_m18)

wfl_internal_zscores_male<- blood_child_imputed_mix %>% 
  filter(bw_sex == 1) %>% 
  mutate(zwfl_internal_del_merge = (weight_del_merge/(1000*length_del_merge) - mean(na.rm = TRUE,weight_del_merge/(1000*length_del_merge)))/sd(na.rm = TRUE,weight_del_merge/(1000*length_del_merge)),
         zwfl_internal_wk3 = (weight_wk3/(1000*length_wk3) - mean(na.rm = TRUE,weight_wk3/(1000*length_wk3)))/sd(na.rm = TRUE,weight_wk3/(1000*length_wk3)),
         zwfl_internal_wk6 = (weight_wk6/(1000*length_wk6) - mean(na.rm = TRUE,weight_wk6/(1000*length_wk6)))/sd(na.rm = TRUE,weight_wk6/(1000*length_wk6)),
         zwfl_internal_m3 = (weight_m3/(1000*length_m3) - mean(na.rm = TRUE,weight_m3/(1000*length_m3)))/sd(na.rm = TRUE,weight_m3/(1000*length_m3)),
         zwfl_internal_m6 = (weight_m6/(1000*length_m6) - mean(na.rm = TRUE,weight_m6/(1000*length_m6)))/sd(na.rm = TRUE,weight_m6/(1000*length_m6)),
         zwfl_internal_m12 = (weight_m12/(1000*length_m12) - mean(na.rm = TRUE,weight_m12/(1000*length_m12)))/sd(na.rm = TRUE,weight_m12/(1000*length_m12)),
         zwfl_internal_m18 = (weight_m18/(1000*length_m18) - mean(na.rm = TRUE,weight_m18/(1000*length_m18)))/sd(na.rm = TRUE,weight_m18/(1000*length_m18)))%>% 
  select(Subject_ID, zwfl_internal_del_merge, zwfl_internal_wk3, zwfl_internal_wk6, zwfl_internal_m3, zwfl_internal_m6, zwfl_internal_m12, zwfl_internal_m18)

wfl_internal_zscores<- rbind(wfl_internal_zscores_female, wfl_internal_zscores_male)




## combine all datasets and add 
blood_child_who_stan_impute_mix<- data.frame(blood_child_imputed_mix[,c("Subject_ID", weight, length, child_age, "bw_sex", "bw_birth_GA")],
                                           blood_child_who_stan_del_merge,
                                           blood_child_who_stan_wk3,
                                           blood_child_who_stan_wk6,
                                           blood_child_who_stan_m3,
                                           blood_child_who_stan_m6,
                                           blood_child_who_stan_m12,
                                           blood_child_who_stan_m18) %>% 
  inner_join(wfl_internal_zscores, by="Subject_ID")


# write_csv(blood_child_who_stan_impute_mix, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_impute_mix_dropwk1_11162023.csv")

blood_child_who_stan_impute_mix<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_impute_mix_dropwk1_11162023.csv")




vis_miss(blood_child_who_stan_impute_mix %>% select(starts_with("zwfl_internal")))
vis_miss(blood_child_who_stan_impute_mix %>% select(starts_with("zwfl_internal")),sort_miss = TRUE)
gg_miss_var(blood_child_who_stan_impute_mix %>% select(starts_with("zwfl_internal")))
















