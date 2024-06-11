
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
library(gtsummary)
library(gt)

#-------------------------------------------------------import data set
whole_cohort<-  read_csv("C:/Users/yaom03/OneDrive - The Mount Sinai Hospital/Documents/Projects/S-PRESTO/input/for paper/whole_cohort_20230316.csv")

whole_cohort$ethnicity_specified_recat<- factor(whole_cohort$ethnicity_specified_recat,
                                            levels = c("Chinese","Malay","Indian"))

whole_cohort$pcv1_highest_education_completed_recat<- factor(whole_cohort$pcv1_highest_education_completed_recat,
                                                         levels=c("University","Primary/Secondary/Post_secondary"))


whole_cohort$pcv1_parity_recat<- factor(whole_cohort$pcv1_parity_recat,
                                    levels=c("0",">= 1"))

whole_cohort$pcv1_smoker<- factor(whole_cohort$pcv1_smoker,
                                        levels=c(0:2),
                                        labels = c("never smoke", "smoker", "ex-smoker"))



#-------------------------------------------------------data preparation


## Restricted to participants with a blood sample and a live birth - blood(328)
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


## only include child anthropometry variables
weight<- c('weight_del_merge', 'weight_wk1','weight_wk3','weight_wk6','weight_m3','weight_m6','weight_m12','weight_m18')
length<- c('length_del_merge', 'length_wk1','length_wk3','length_wk6','length_m3','length_m6','length_m12','length_m18')
child_age<- c('Days_since_birth_del_merge','Days_since_birth_wk1','Days_since_birth_wk3','Days_since_birth_wk6','Days_since_birth_m3','Days_since_birth_m6','Days_since_birth_m12','Days_since_birth_m18')




## Restricted to participants who have at least delivery visit and m18 visit - blood_child_del_m18(240)
blood_child<- blood[,c("Subject_ID", "bw_sex", "bw_birth_GA", weight, length, child_age,'age_at_recruitment', 'pcv1_smoker', 'pcv1_highest_education_completed_recat','ethnicity_specified_recat','pcv1_parity_recat','pcv1_bmi')]
blood_child_del_m18<- blood_child[complete.cases(blood_child[,c('weight_del_merge', 'weight_m18', "length_del_merge", "length_m18", "Days_since_birth_del_merge", "Days_since_birth_m18")]),]
#

vis_miss(blood_child_del_m18)
vis_miss(blood_child_del_m18,sort_miss = TRUE)
gg_miss_var(blood_child_del_m18)



## completed cases - blood_child_complete(171)
blood_child_complete<- blood_child[complete.cases(blood_child),]


## completed cases after we drop wk1 measures - blood_child_del_m18_nowk1 (187)
blood_child_del_m18_nowk1<- blood_child[complete.cases(subset(blood_child, select = -c(weight_wk1, length_wk1, Days_since_birth_wk1))),] %>% 
                            mutate(complete= "complete")
blood_child_del_m18<- blood_child_del_m18 %>% 
                      left_join(blood_child_del_m18_nowk1[, c("Subject_ID", "complete")], by="Subject_ID") %>% 
                      mutate(complete_ind = if_else(is.na(complete)==TRUE, "not complete", "complete"))


## completed cases after we drop wk1 & wk3 measures - blood_child_del_m18_nowk1_3 (191)
blood_child_del_m18_nowk1_3<- blood_child[complete.cases(subset(blood_child, select = -c(weight_wk1, length_wk1, Days_since_birth_wk1, weight_wk3, length_wk3, Days_since_birth_wk3))),]


## Restricted to participants who came at least one visit - blood_child_v1(324)
### weights
blood_child_v1_step1 = blood_child[!(is.na(blood_child$weight_del_merge) & 
                              is.na(blood_child$weight_wk1) &
                              is.na(blood_child$weight_wk3) &
                              is.na(blood_child$weight_wk6) &
                              is.na(blood_child$weight_m3) &
                              is.na(blood_child$weight_m6) &
                              is.na(blood_child$weight_m12) &
                              is.na(blood_child$weight_m18)), ]
### lengths
blood_child_v1_step2 = blood_child_v1_step1[!(is.na(blood_child_v1_step1$length_del_merge) &
                              is.na(blood_child_v1_step1$length_wk1) &
                              is.na(blood_child_v1_step1$length_wk3) &
                              is.na(blood_child_v1_step1$length_wk6) &
                              is.na(blood_child_v1_step1$length_m3) &
                              is.na(blood_child_v1_step1$length_m6) &
                              is.na(blood_child_v1_step1$length_m12) &
                              is.na(blood_child_v1_step1$length_m18)), ]
### days since birth
blood_child_v1 = blood_child_v1_step2[!(is.na(blood_child_v1_step2$Days_since_birth_del_merge) & 
                              is.na(blood_child_v1_step2$Days_since_birth_wk1) &
                              is.na(blood_child_v1_step2$Days_since_birth_wk3) &
                              is.na(blood_child_v1_step2$Days_since_birth_wk6) &
                              is.na(blood_child_v1_step2$Days_since_birth_m3) &
                              is.na(blood_child_v1_step2$Days_since_birth_m6) &
                              is.na(blood_child_v1_step2$Days_since_birth_m12) &
                              is.na(blood_child_v1_step2$Days_since_birth_m18)), ]


#########################
##########table##########
#########################


blood_table<- blood %>% 
              tbl_summary(
                include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                            ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                            pcv1_bmi),
                missing_text = "Missing"
              ) %>% 
              bold_labels()





blood_child_v1_table<- blood_child_v1 %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi),
    missing_text = "Missing"
  ) %>% 
  bold_labels()





blood_child_del_m18_table<- blood_child_del_m18 %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi),
    missing_text = "Missing"
  ) %>% 
  bold_labels()




blood_child_complete_table<- blood_child_complete %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi),
    missing_text = "Missing"
  ) %>% 
  bold_labels()




blood_child_del_m18_nowk1_table<- blood_child_del_m18_nowk1 %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi),
    missing_text = "Missing"
  ) %>% 
  bold_labels()





blood_child_del_m18_nowk1_3_table<- blood_child_del_m18_nowk1_3 %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi),
    missing_text = "Missing"
  ) %>% 
  bold_labels()






table<- tbl_merge(tbls = list(blood_table, blood_child_v1_table, blood_child_del_m18_table, 
                           blood_child_complete_table, blood_child_del_m18_nowk1_table, blood_child_del_m18_nowk1_3_table),
                  tab_spanner = c("**Overall participants (have a live birth)**", "**Participants who have at least one visit measurement**",
                                  "**Participants who have completed delivery and m18 measurements**", "**Participants who have completed all measurements**",
                                  "**Participants who have completed all measurements (drop wk1)**", "**Participants who have completed all measurements (drop wk1 & 3)**")
                  )


table





blood_child_del_m18 %>% 
  tbl_summary(
    include = c(age_at_recruitment, pcv1_highest_education_completed_recat, 
                ethnicity_specified_recat, pcv1_parity_recat, pcv1_smoker,
                pcv1_bmi, complete_ind),
    by = complete_ind,
    missing_text = "Missing"
  ) %>% 
  bold_labels() %>% 
  add_p()










