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
weight<- c('weight_del_merge', 'weight_wk1','weight_wk3','weight_wk6','weight_m3','weight_m6','weight_m12','weight_m18')
length<- c('length_del_merge', 'length_wk1','length_wk3','length_wk6','length_m3','length_m6','length_m12','length_m18')
child_age<- c('Days_since_birth_del_merge','Days_since_birth_wk1','Days_since_birth_wk3','Days_since_birth_wk6','Days_since_birth_m3','Days_since_birth_m6','Days_since_birth_m12','Days_since_birth_m18')


blood_child<- blood[,c("Subject_ID", "bw_sex", "bw_birth_GA", weight, length, child_age,'age_at_recruitment', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat','pcv1_bmi')]
blood_child_del_m18<- blood_child[complete.cases(blood_child[,c('weight_del_merge', 'weight_m18', "length_del_merge", "length_m18", "Days_since_birth_del_merge", "Days_since_birth_m18")]),]

vis_miss(blood_child_del_m18)
vis_miss(blood_child_del_m18,sort_miss = TRUE)
gg_miss_var(blood_child_del_m18)


blood_child_complete<- blood_child[complete.cases(blood_child),]


#------------------------------------------------------- imputation
###------------------------------------------------------------------------------ weights & length & bw_sex & GA
str(blood_child_del_m18)

blood_child_del_m18$pcv1_highest_education_completed_recat<- as.factor(blood_child_del_m18$pcv1_highest_education_completed_recat)
blood_child_del_m18$ethnicity_specified_recat<- as.factor(blood_child_del_m18$ethnicity_specified_recat)
blood_child_del_m18$pcv1_parity_recat<- as.factor(blood_child_del_m18$pcv1_parity_recat)

str(blood_child_del_m18)
#### to check missingness

blood_child_del_m18_for_impute<- subset(blood_child_del_m18, select = -(Subject_ID))
apply(blood_child_del_m18_for_impute, 2, function(x) sum(is.na(x))) 
md.pattern(blood_child_del_m18_for_impute)
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



blood_child_imputed<- data.frame(blood_child_del_m18[, c("Subject_ID")],
                                 blood_child_imputed) %>% 
                      mutate(measure="L")


# write_csv(blood_child_imputed, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_imputed.csv")


#------------------------------------------------------- restrict to completed cases
colSums(is.na(blood_child_complete))

blood_child_complete<- blood_child_complete %>% 
  mutate(measure="L")

# write_csv(blood_child_complete, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_complete.csv")




#------------------------------------------------------- WHO child growth standards
#----------------- imputed data

## delivery (merge hospital records & delivery visit)
blood_child_who_stan_del_merge<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_del_merge, 
                 weight = weight_del_merge/1000, 
                 lenhei = length_del_merge,
                 measure = measure)
)[,c("zwei", "zlen", "zbmi", "zwfl")] 

names(blood_child_who_stan_del_merge)<- c("zweight_del_merge", "zlength_del_merge", "zbmi_del_merge", "zwfl_WHO_del_merge")


## at wk1
blood_child_who_stan_wk1<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk1, 
                 weight = weight_wk1/1000, 
                 lenhei = length_wk1,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk1)<- c("zweight_wk1", "zlength_wk1", "zbmi_wk1", "zwfl_WHO_wk1")

## at wk3
blood_child_who_stan_wk3<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk3, 
                 weight = weight_wk3/1000, 
                 lenhei = length_wk3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk3)<- c("zweight_wk3", "zlength_wk3", "zbmi_wk3", "zwfl_WHO_wk3")

## at wk6
blood_child_who_stan_wk6<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk6, 
                 weight = weight_wk6/1000, 
                 lenhei = length_wk6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk6)<- c("zweight_wk6", "zlength_wk6", "zbmi_wk6", "zwfl_WHO_wk6")

## at m3
blood_child_who_stan_m3<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m3, 
                 weight = weight_m3/1000, 
                 lenhei = length_m3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m3)<- c("zweight_m3", "zlength_m3", "zbmi_m3", "zwfl_WHO_m3")

## at m6
blood_child_who_stan_m6<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m6, 
                 weight = weight_m6/1000, 
                 lenhei = length_m6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m6)<- c("zweight_m6", "zlength_m6", "zbmi_m6", "zwfl_WHO_m6")

## at m12
blood_child_who_stan_m12<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m12, 
                 weight = weight_m12/1000, 
                 lenhei = length_m12,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]


names(blood_child_who_stan_m12)<- c("zweight_m12", "zlength_m12", "zbmi_m12", "zwfl_WHO_m12")

## at m18
blood_child_who_stan_m18<- with(
  blood_child_imputed,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m18, 
                 weight = weight_m18/1000, 
                 lenhei = length_m18,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m18)<- c("zweight_m18", "zlength_m18", "zbmi_m18", "zwfl_WHO_m18")

# Internal z-scores for weight-to-height ratio
wfl_internal_zscores<- blood_child_imputed %>% 
  transmute(zwfl_internal_del_merge = (weight_del_merge/(1000*length_del_merge) - mean(weight_del_merge/(1000*length_del_merge)))/sd(weight_del_merge/(1000*length_del_merge)),
            zwfl_internal_wk1 = (weight_wk1/(1000*length_wk1) - mean(weight_wk1/(1000*length_wk1)))/sd(weight_wk1/(1000*length_wk1)),
            zwfl_internal_wk3 = (weight_wk3/(1000*length_wk3) - mean(weight_wk3/(1000*length_wk3)))/sd(weight_wk3/(1000*length_wk3)),
            zwfl_internal_wk6 = (weight_wk6/(1000*length_wk6) - mean(weight_wk6/(1000*length_wk6)))/sd(weight_wk6/(1000*length_wk6)),
            zwfl_internal_m3 = (weight_m3/(1000*length_m3) - mean(weight_m3/(1000*length_m3)))/sd(weight_m3/(1000*length_m3)),
            zwfl_internal_m6 = (weight_m6/(1000*length_m6) - mean(weight_m6/(1000*length_m6)))/sd(weight_m6/(1000*length_m6)),
            zwfl_internal_m12 = (weight_m12/(1000*length_m12) - mean(weight_m12/(1000*length_m12)))/sd(weight_m12/(1000*length_m12)),
            zwfl_internal_m18 = (weight_m18/(1000*length_m18) - mean(weight_m18/(1000*length_m18)))/sd(weight_m18/(1000*length_m18)))

## combine all datasets and add 
blood_child_who_stan<- data.frame(blood_child_imputed[,c("Subject_ID", weight, length, child_age, "bw_sex", "bw_birth_GA")],
                                  blood_child_who_stan_del_merge,
                                  blood_child_who_stan_wk1,
                                  blood_child_who_stan_wk3,
                                  blood_child_who_stan_wk6,
                                  blood_child_who_stan_m3,
                                  blood_child_who_stan_m6,
                                  blood_child_who_stan_m12,
                                  blood_child_who_stan_m18,
                                  wfl_internal_zscores)



# write_csv(blood_child_who_stan, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_imputed_05262023.csv")

#----------------- complete cases data

## delivery (merge hospital records & delivery visit)
blood_child_who_stan_del_merge<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_del_merge, 
                 weight = weight_del_merge/1000, 
                 lenhei = length_del_merge,
                 measure = measure)
)[,c("zwei", "zlen", "zbmi", "zwfl")] 

names(blood_child_who_stan_del_merge)<- c("zweight_del_merge", "zlength_del_merge", "zbmi_del_merge", "zwfl_WHO_del_merge")


## at wk1
blood_child_who_stan_wk1<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk1, 
                 weight = weight_wk1/1000, 
                 lenhei = length_wk1,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk1)<- c("zweight_wk1", "zlength_wk1", "zbmi_wk1", "zwfl_WHO_wk1")

## at wk3
blood_child_who_stan_wk3<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk3, 
                 weight = weight_wk3/1000, 
                 lenhei = length_wk3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk3)<- c("zweight_wk3", "zlength_wk3", "zbmi_wk3", "zwfl_WHO_wk3")

## at wk6
blood_child_who_stan_wk6<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_wk6, 
                 weight = weight_wk6/1000, 
                 lenhei = length_wk6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_wk6)<- c("zweight_wk6", "zlength_wk6", "zbmi_wk6", "zwfl_WHO_wk6")

## at m3
blood_child_who_stan_m3<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m3, 
                 weight = weight_m3/1000, 
                 lenhei = length_m3,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m3)<- c("zweight_m3", "zlength_m3", "zbmi_m3", "zwfl_WHO_m3")

## at m6
blood_child_who_stan_m6<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m6, 
                 weight = weight_m6/1000, 
                 lenhei = length_m6,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m6)<- c("zweight_m6", "zlength_m6", "zbmi_m6", "zwfl_WHO_m6")

## at m12
blood_child_who_stan_m12<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m12, 
                 weight = weight_m12/1000, 
                 lenhei = length_m12,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]


names(blood_child_who_stan_m12)<- c("zweight_m12", "zlength_m12", "zbmi_m12", "zwfl_WHO_m12")

## at m18
blood_child_who_stan_m18<- with(
  blood_child_complete,
  anthro_zscores(sex = bw_sex, 
                 age = Days_since_birth_m18, 
                 weight = weight_m18/1000, 
                 lenhei = length_m18,
                 measure = "L")
)[,c("zwei", "zlen", "zbmi", "zwfl")]

names(blood_child_who_stan_m18)<- c("zweight_m18", "zlength_m18", "zbmi_m18", "zwfl_WHO_m18")

# Internal z-scores for weight-to-height ratio
wfl_internal_zscores<- blood_child_complete %>% 
  transmute(zwfl_internal_del_merge = (weight_del_merge/(1000*length_del_merge) - mean(weight_del_merge/(1000*length_del_merge)))/sd(weight_del_merge/(1000*length_del_merge)),
            zwfl_internal_wk1 = (weight_wk1/(1000*length_wk1) - mean(weight_wk1/(1000*length_wk1)))/sd(weight_wk1/(1000*length_wk1)),
            zwfl_internal_wk3 = (weight_wk3/(1000*length_wk3) - mean(weight_wk3/(1000*length_wk3)))/sd(weight_wk3/(1000*length_wk3)),
            zwfl_internal_wk6 = (weight_wk6/(1000*length_wk6) - mean(weight_wk6/(1000*length_wk6)))/sd(weight_wk6/(1000*length_wk6)),
            zwfl_internal_m3 = (weight_m3/(1000*length_m3) - mean(weight_m3/(1000*length_m3)))/sd(weight_m3/(1000*length_m3)),
            zwfl_internal_m6 = (weight_m6/(1000*length_m6) - mean(weight_m6/(1000*length_m6)))/sd(weight_m6/(1000*length_m6)),
            zwfl_internal_m12 = (weight_m12/(1000*length_m12) - mean(weight_m12/(1000*length_m12)))/sd(weight_m12/(1000*length_m12)),
            zwfl_internal_m18 = (weight_m18/(1000*length_m18) - mean(weight_m18/(1000*length_m18)))/sd(weight_m18/(1000*length_m18)))

## combine all datasets and add 
blood_child_who_stan_complete<- data.frame(blood_child_complete[,c("Subject_ID", weight, length, child_age, "bw_sex", "bw_birth_GA")],
                                  blood_child_who_stan_del_merge,
                                  blood_child_who_stan_wk1,
                                  blood_child_who_stan_wk3,
                                  blood_child_who_stan_wk6,
                                  blood_child_who_stan_m3,
                                  blood_child_who_stan_m6,
                                  blood_child_who_stan_m12,
                                  blood_child_who_stan_m18,
                                  wfl_internal_zscores)



# write_csv(blood_child_who_stan_complete, "C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/anthropometry/blood_child_who_stan_complete_05262023.csv")















