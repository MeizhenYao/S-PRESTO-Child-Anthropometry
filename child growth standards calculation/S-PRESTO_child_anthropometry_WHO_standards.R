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

#-------------------------------------------------------import data set
whole_cohort<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/whole_cohort_20230316.csv")


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

## only include child anthropometry variables
weight<- c('weight_del', 'weight_wk1','weight_wk3','weight_wk6','weight_m3','weight_m6','weight_m12','weight_m18')
length<- c('length_del', 'length_wk1','length_wk3','length_wk6','length_m3','length_m6','length_m12','length_m18')
child_age<- c('Days_since_birth_del','Days_since_birth_wk1','Days_since_birth_wk3','Days_since_birth_wk6','Days_since_birth_m3','Days_since_birth_m6','Days_since_birth_m12','Days_since_birth_m18')


blood_child<- blood[,c("Subject_ID", "bw_sex", "bw_birth_GA", weight, length, child_age)]


## assign NA to '-1' in days_since_Del
blood_child$Days_since_birth_del<- ifelse(blood_child$Days_since_birth_del == -1, NA, blood_child$Days_since_birth_del)

## Restricted to participants who came at least one visit
### weights
blood_child = blood_child[!(is.na(blood_child$weight_del) &
                              is.na(blood_child$weight_wk1) &
                              is.na(blood_child$weight_wk3) &
                              is.na(blood_child$weight_wk6) &
                              is.na(blood_child$weight_m3) &
                              is.na(blood_child$weight_m6) &
                              is.na(blood_child$weight_m12) &
                              is.na(blood_child$weight_m18)), ]
### lengths
blood_child = blood_child[!(is.na(blood_child$length_del) &
                              is.na(blood_child$length_wk1) &
                              is.na(blood_child$length_wk3) &
                              is.na(blood_child$length_wk6) &
                              is.na(blood_child$length_m3) &
                              is.na(blood_child$length_m6) &
                              is.na(blood_child$length_m12) &
                              is.na(blood_child$length_m18)), ]
### days since birth
blood_child = blood_child[!(is.na(blood_child$Days_since_birth_del) &
                              is.na(blood_child$Days_since_birth_wk1) &
                              is.na(blood_child$Days_since_birth_wk3) &
                              is.na(blood_child$Days_since_birth_wk6) &
                              is.na(blood_child$Days_since_birth_m3) &
                              is.na(blood_child$Days_since_birth_m6) &
                              is.na(blood_child$Days_since_birth_m12) &
                              is.na(blood_child$Days_since_birth_m18)), ]

# write_csv(blood_child, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_before_imputed.csv")

#------------------------------------------------------- imputation (prediction model only include child anthropometry variables)
###------------------------------------------------------------------------------ weights & length & bw_sex & GA
blood_child_weights_lengths<- blood_child[, c(weight, length, "bw_sex", "bw_birth_GA")]

#### to check missingness
apply(blood_child_weights_lengths, 2, function(x) sum(is.na(x)))  

#### Initialize the Imputation
init = mice(blood_child_weights_lengths, maxit=0) 
meth = init$method
predM = init$predictorMatrix



####  Specify a method to be used for imputation for some selected variables 
#####   used Random Forest(rf) for selected variables
meth[c(weight, length, "bw_sex", "bw_birth_GA")] = "rf"


####   Now Run your Main Imputation using "Random Forest"
#####   m = 5 creates 5 different imputed datasets 
set.seed(621052)
blood_child_weights_lengths_imputing = mice(blood_child_weights_lengths, method=meth, predictorMatrix=predM, m=5, ntree = 50)
#####    Use the last completed data set 
blood_child_weights_lengths_imputed <- complete(blood_child_weights_lengths_imputing, action=5)


####    Check if you actually imputed all the variables you wanted to impute
sapply(blood_child_weights_lengths_imputed, function(x) sum(is.na(x)))


###------------------------------------------------------------------------------ days since birth
blood_child_days<- blood_child[, child_age]

#### to check missingness
apply(blood_child_days, 2, function(x) sum(is.na(x)))  

#### Initialize the Imputation
init = mice(blood_child_days, maxit=0) 
meth = init$method
predM = init$predictorMatrix



####  Specify a method to be used for imputation for some selected variables 
meth[child_age] = "pmm"


####   Now Run your Main Imputation 
set.seed(320156)
blood_child_days_imputing = mice(blood_child_days, method=meth, predictorMatrix=predM, m=7, maxit = 20)
#####    Use the last completed data set 
blood_child_days_imputed <- complete(blood_child_days_imputing, action=7)


####    Check if you actually imputed all the variables you wanted to impute
sapply(blood_child_days_imputed, function(x) sum(is.na(x)))



blood_child_imputed<- data.frame(blood_child[, c("Subject_ID")],
                                 blood_child_weights_lengths_imputed,
                                 blood_child_days_imputed) %>% 
  mutate(measure="L")


# write_csv(blood_child_imputed, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_imputed.csv")


#------------------------------------------------------- WHO child growth standards

# WHO child growth standards
blood_child_imputed<- read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_imputed.csv")

## delivery
blood_child_who_stan_del<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_del, 
                 weight = weight_del/1000, 
                 lenhei = length_del,
                 measure = measure)
)[,c("zwei", "zlen", "zbmi", "zwfl")] 

names(blood_child_who_stan_del)<- c("zweight_del", "zlength_del", "zbmi_del", "zwfl_del")



## at wk1
blood_child_who_stan_wk1<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk1, 
                 weight = weight_wk1/1000, 
                 lenhei = length_wk1,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk1)<- c("zweight_wk1", "zlength_wk1", "zbmi_wk1", "zwfl_wk1")

## at wk3
blood_child_who_stan_wk3<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk3, 
                 weight = weight_wk3/1000, 
                 lenhei = length_wk3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk3)<- c("zweight_wk3", "zlength_wk3", "zbmi_wk3", "zwfl_wk3")

## at wk6
blood_child_who_stan_wk6<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk6, 
                 weight = weight_wk6/1000, 
                 lenhei = length_wk6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk6)<- c("zweight_wk6", "zlength_wk6", "zbmi_wk6", "zwfl_wk6")

## at m3
blood_child_who_stan_m3<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m3, 
                 weight = weight_m3/1000, 
                 lenhei = length_m3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m3)<- c("zweight_m3", "zlength_m3", "zbmi_m3", "zwfl_m3")

## at m6
blood_child_who_stan_m6<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m6, 
                 weight = weight_m6/1000, 
                 lenhei = length_m6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m6)<- c("zweight_m6", "zlength_m6", "zbmi_m6", "zwfl_m6")

## at m12
blood_child_who_stan_m12<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m12, 
                 weight = weight_m12/1000, 
                 lenhei = length_m12,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]


names(blood_child_who_stan_m12)<- c("zweight_m12", "zlength_m12", "zbmi_m12", "zwfl_m12")

## at m18
blood_child_who_stan_m18<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m18, 
                 weight = weight_m18/1000, 
                 lenhei = length_m18,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m18)<- c("zweight_m18", "zlength_m18", "zbmi_m18", "zwfl_m18")

# Internal z-scores for weight-to-height ratio
wfl_internal_zscores<- blood_child_imputed %>% 
  transmute(zwfl_internal_del = (weight_del/(1000*length_del) - mean(weight_del/(1000*length_del)))/sd(weight_del/(1000*length_del)),
            zwfl_internal_wk1 = (weight_wk1/(1000*length_wk1) - mean(weight_wk1/(1000*length_wk1)))/sd(weight_wk1/(1000*length_wk1)),
            zwfl_internal_wk3 = (weight_wk3/(1000*length_wk3) - mean(weight_wk3/(1000*length_wk3)))/sd(weight_wk3/(1000*length_wk3)),
            zwfl_internal_wk6 = (weight_wk6/(1000*length_wk6) - mean(weight_wk6/(1000*length_wk6)))/sd(weight_wk6/(1000*length_wk6)),
            zwfl_internal_m3 = (weight_m3/(1000*length_m3) - mean(weight_m3/(1000*length_m3)))/sd(weight_m3/(1000*length_m3)),
            zwfl_internal_m6 = (weight_m6/(1000*length_m6) - mean(weight_m6/(1000*length_m6)))/sd(weight_m6/(1000*length_m6)),
            zwfl_internal_m12 = (weight_m12/(1000*length_m12) - mean(weight_m12/(1000*length_m12)))/sd(weight_m12/(1000*length_m12)),
            zwfl_internal_m18 = (weight_m18/(1000*length_m18) - mean(weight_m18/(1000*length_m18)))/sd(weight_m18/(1000*length_m18)))

## combine all datasets and add 
blood_child_who_stan<- data.frame(blood_child_imputed[,c("Subject_ID", child_age, "bw_sex", "bw_birth_GA")],
                                  blood_child_who_stan_del,
                                  blood_child_who_stan_wk1,
                                  blood_child_who_stan_wk3,
                                  blood_child_who_stan_wk6,
                                  blood_child_who_stan_m3,
                                  blood_child_who_stan_m6,
                                  blood_child_who_stan_m12,
                                  blood_child_who_stan_m18,
                                  wfl_internal_zscores)



write_xlsx(blood_child_who_stan, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_03232023.xlsx")

#################################################################### 
####################################################################
####################################################################
# 
# blood_child_who_stan<-read_excel("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_03082023.xlsx")
# blood_child_imputed<-read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_imputed.csv")
# 
# sapply(blood_child_who_stan, function(x) sum(is.na(x)))
# 
# 
# blood_child_check<- blood_child_who_stan %>%
#                     inner_join((blood_child_imputed %>% 
#                                 select(-starts_with("Days_since_birth"))), by="Subject_ID") %>%
#                     filter(is.na(zwfl_del)==TRUE | is.na(zwfl_wk1)==TRUE) %>% 
#                     select(Subject_ID, ends_with("_del"), ends_with("_wk1"))
# 
# # %>%
# #                     select(Subject_ID,zwfl_del, zwfl_wk1, length_del, length_wk1)
# 
# anthro_zscores(
#    sex = 1, age = 92,
#    weight = 2.8, lenhei = 44.99999999999999, measure="L" , armc=10.85)
# 
# 
# 
# 
# 
# 
