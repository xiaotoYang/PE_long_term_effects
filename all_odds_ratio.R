# odds ratio all
# Xiaotong Yang 

# calculates: 
#1)Overall OR on UM dataset
#2)OR by race on UM dataset
#3)OR by PE severity on UM dataset
#4)OR by fetal sex on UM dataset

#`overall OR---------------------------------------------------------------------
setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables")

load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/case_list_for_or.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/control_list_for_or.RData")

library(dplyr)
target = c("HTN", "DMcx", "DM", "Renal", "Obesity")
result = data.frame()

for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
                AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
                Obesity + RenalFailure + DiabetesUncomplicated,  family = binomial(link = "logit"), data = data1)
  result = result %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}


result
write.csv(result, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result.csv")


# OR by race--------------------------------------------------------
result_caucasian = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  data1 = data1%>%filter(Caucasian==1)
  for(j in c(1:nrow(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
  result_caucasian  = result_caucasian  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_caucasian 

result_AA = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  data1 = data1%>%filter(`African American`==1)
  for(j in c(1:nrow(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+ DiabetesUncomplicated+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure ,  family = binomial(link = "logit"), data = data1)
  result_AA  = result_AA  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_AA
write.csv(result_caucasian, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_caucasian.csv")
write.csv(result_AA, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_AA.csv")

data = as.data.frame(rbind(result_caucasian,result_AA))

data$race = c(rep("Caucasian", 6), rep("AA", 6))
data
figure2 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), xmax=exp(LogitCoef+1.96*CoefStd), color = race, group = race)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/all_odds_ratio_race_UM.png", height = 4.8, width = 7.2, dpi = 600)


# Odds ratio by PE severity----------------------------------------------------------
library(icd)
library(touch)
library(tidyverse)
diag = read.csv("/nfs/turbo/umms-lgarmire2/haomingz-copy/long_term_effect_PE_3/case/DiagnosesComprehensiveAll.csv")
diag_convert = diag

## Convert to ICD-10
diag_convert[diag_convert$Lexicon == "ICD9","TermCodeMapped"] = icd_map(diag_convert[diag_convert$Lexicon == "ICD9","TermCodeMapped"])
diag_convert$TermCodeMapped = gsub(",.*", "", diag_convert$TermCodeMapped)
diag_convert$TermCodeMapped[diag_convert$Lexicon == "ICD9"] = short_to_decimal(diag_convert$TermCodeMapped[diag_convert$Lexicon == "ICD9"])

severe_pe = diag_convert %>%
  group_by(DeID_PatientID) %>%
  filter(any(grepl("O14.1", TermCodeMapped))) %>%
  select(DeID_PatientID)%>%
  slice(1)
mild_pe = diag_convert %>%
  group_by(DeID_PatientID) %>%
  #filter(any(grepl("O14.0", TermCodeMapped))) %>%
  filter(!any(grepl("O14.1", TermCodeMapped)))%>%
  filter(!any(grepl("O14.2", TermCodeMapped)))%>%
  select(DeID_PatientID)%>%
  slice(1)

#hazard ratio
result_severe = data.frame()
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(severe_pe)
  data1 <- rbind(case, control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure + CongestiveHeartFailure+Caucasian,  family = binomial(link = "logit"), data = data1)
  result_severe  = result_severe %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}

result_severe 

result_mild = data.frame()
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(mild_pe)
  data1 <- rbind(case, control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure + CongestiveHeartFailure+Caucasian,  family = binomial(link = "logit"), data = data1)
  result_mild  = result_mild %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_mild

write.csv(result_severe, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_severe.csv")
write.csv(result_mild, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_mild.csv")

data = as.data.frame(rbind(result_severe,result_mild))
data$Severity = c(rep("severe", 6), rep("mild", 6))
data
figure3 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), xmax=exp(LogitCoef+1.96*CoefStd), color = Severity, group = Severity)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure3

ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/HazardRatio_severity.png", height = 4.8, width = 7.2, dpi = 600)




#calculate odds ratio for male/female baby-------------------------------------

library(dplyr)
#load previous OR data
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/case_list_for_or.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/control_list_for_or.RData")
#load baby info
baby_case = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/Stork_Pregnancy_Outcomes_case.csv")
baby_control = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/Stork_Pregnancy_Outcomes_control.csv")
#load sga info
pe_sga = read.csv("baby_sex_and_sga/sga_baby_demogrpahics.csv")
control_sga = read.csv("baby_sex_and_sga/control_baby_sga_demographics.csv")

#join diagnosis record and delivery record-------------------------------------------------------------
PE_patient = case_survival_list$HTN%>%
  select(DeID_PatientID, start_time, PE_age)%>%
  inner_join(baby_case)
dim(PE_patient)  
PE_patient$DeID_Delivery_Time = as.Date(PE_patient$DeID_Delivery_Time, format = "%m/%d/%Y %H:%M")

#ensure baby record and maternal record are from the same pregnancy
#by restrict PE onset time is at most 180 d before delivery and 30 d after delivery
PE_patient = PE_patient%>%filter(DeID_Delivery_Time - start_time<180 & DeID_Delivery_Time - start_time>-30)
dim(PE_patient)

controls = control_survival_list$HTN%>%
  select(DeID_PatientID, start_time, PE_age)%>%
  inner_join(baby_control)
dim(controls)  
controls$DeID_Delivery_Time = as.Date(controls$DeID_Delivery_Time, format = "%m/%d/%Y %H:%M")

#ensure baby record and maternal record are from the same pregnancy
#by restrict PE onset time is at most 180 d before delivery and 30 d after delivery
controls = controls%>%filter(DeID_Delivery_Time - start_time<300 & DeID_Delivery_Time - start_time>-30)
dim(controls)

write.csv(PE_patient$DeID_PatientID_Baby, file = "baby_sex_and_sga/PE_baby_list.csv", row.names = F)
write.csv(controls$DeID_PatientID_Baby, file = "baby_sex_and_sga/control_baby_list.csv", row.names = F)
twins_diff_gender <- PE_patient %>%
  group_by(DeID_PatientID, DeID_Delivery_Time) %>%
  filter(n() >= 2) %>%
  filter(n_distinct(BABY_SEX) > 1) %>%
  ungroup()

twins_diff_gender_control <- controls %>%
  group_by(DeID_PatientID, DeID_Delivery_Time) %>%
  filter(n() >= 2) %>%
  filter(n_distinct(BABY_SEX) > 1) %>%
  ungroup()

PE_no_twins = PE_patient%>%filter(!DeID_PatientID_Baby%in%twins_diff_gender$DeID_PatientID_Baby)
control_no_twins =  controls%>%filter(!DeID_PatientID_Baby%in%twins_diff_gender_control$DeID_PatientID_Baby)

target = c("HTN", "Renal")
result_female_baby = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_no_twins%>%filter(BABY_SEX=="FEMALE")%>%select(DeID_PatientID)), 
                 control_survival_list[[target[i]]]%>%inner_join(control_no_twins%>%filter(BABY_SEX=="FEMALE")%>%select(DeID_PatientID)))
  for(j in c(1:nrow(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
  result_female_baby  = result_female_baby  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_female_baby 

result_male_baby = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_no_twins%>%filter(BABY_SEX=="MALE")%>%select(DeID_PatientID)), 
                 control_survival_list[[target[i]]]%>%inner_join(control_no_twins%>%filter(BABY_SEX=="MALE")%>%select(DeID_PatientID)))
  for(j in c(1:nrow(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+ DiabetesUncomplicated+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure ,  family = binomial(link = "logit"), data = data1)
  result_male_baby  = result_male_baby  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_male_baby
write.csv(result_female_baby, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_female_baby.csv")
write.csv(result_male_baby, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_male_baby.csv")

data = as.data.frame(rbind(result_female_baby,result_male_baby))

data$baby_sex = c(rep("Female", 6), rep("Male", 6))

library(ggplot2)
figure2 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = baby_sex, group = baby_sex)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/baby_sex_OR.png", height = 3, width = 6, dpi = 600)

