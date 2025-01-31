#baby sex and sga
#xiaotong yang
#01/21/2025

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

#add sga info-----------------------------------------------------------------------
PE_patient$SGA = ifelse(PE_patient$DeID_PatientID_Baby%in%pe_sga$DeID_PatientID, 1, 0)
controls$SGA = ifelse(controls$DeID_PatientID_Baby%in%control_sga$DeID_PatientID, 1, 0)

save(PE_patient, file = "baby_sex_and_sga/PE_patient.RData")
save(controls, file = "baby_sex_and_sga/controls.RData")


#calculate odds ratio for male/female baby-------------------------------------
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

target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
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


#SGA-----------------------------------------------------------------------

target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
result_sga = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_patient%>%filter(SGA == 1)%>%select(DeID_PatientID)),
                 control_survival_list[[target[i]]]%>%inner_join(controls%>%filter(SGA == 0)%>%select(DeID_PatientID)))
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
  result_sga  = result_sga  %>%
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_sga

result_no_sga = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_patient%>%filter(SGA==0)%>%select(DeID_PatientID)),
                 control_survival_list[[target[i]]]%>%inner_join(controls%>%filter(SGA==0)%>%select(DeID_PatientID)))
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+ DiabetesUncomplicated+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure ,  family = binomial(link = "logit"), data = data1)
  result_no_sga  = result_no_sga  %>%
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_no_sga
write.csv(result_sga, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_sga.csv")
write.csv(result_no_sga, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_no_sga.csv")

data = as.data.frame(rbind(result_sga,result_no_sga))

data$SGA = c(rep("SGA", 6), rep("No SGA", 6))

library(ggplot2)
figure3 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd),
                                xmax=exp(LogitCoef+1.96*CoefStd), color = SGA, group = SGA)) +
  theme_bw()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  xlim(0, 24)
  
#scale_x_continuous(breaks = c(1,2,4,6))
figure3
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/sga_vs_no_sga.png", height = 3, width = 6, dpi = 600)

#adjust for HTN ------------------------------
target = c("DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
HTN_case = case_survival_list$HTN$status
HTN_control = control_survival_list$HTN$status
result_adjust_htn = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(data.frame(case_survival_list[[target[i]]], "HTN" = HTN_case),
                 data.frame(control_survival_list[[target[i]]], "HTN" = HTN_control))
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated+HTN ,  family = binomial(link = "logit"), data = data1)
  result_adjust_htn  = result_adjust_htn  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_adjust_htn
write.csv(result_adjust_htn, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/result_adjust_htn.csv")
UM_result <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result.csv")

library(ggplot2)
UM_result = UM_result[2:6, 2:5]#emit htn and rank column
data = as.data.frame(rbind(result_adjust_htn,UM_result))
data$setting = c(rep("Adjust_for_HTN", 5), rep("No_adjustment", 5))
figure2 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd),
                                xmax=exp(LogitCoef+1.96*CoefStd), color = setting, group = setting)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6))
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/adjust_for_htn_result2.png", height = 3, width = 6, dpi = 600)


#PRETERM PE---------------------------------------------------------
target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
result_preterm = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_patient%>%filter(GESTATIONAL_AGE<36*7)%>%select(DeID_PatientID)),
                 control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
  result_preterm  = result_preterm  %>%
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_preterm

result_fullterm = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]]%>%inner_join(PE_patient%>%filter(GESTATIONAL_AGE>=36*7)%>%select(DeID_PatientID)),
                       control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+ DiabetesUncomplicated+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure ,  family = binomial(link = "logit"), data = data1)
  result_fullterm  = result_fullterm  %>%
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_fullterm
write.csv(result_preterm, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_preterm.csv")
write.csv(result_fullterm, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/result_fullterm.csv")

data = as.data.frame(rbind(result_preterm,result_fullterm))

data$subtype = c(rep("Preterm", 6), rep("Fullterm", 6))

library(ggplot2)
figure2 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd),
                                xmax=exp(LogitCoef+1.96*CoefStd), color = subtype, group = subtype)) +
  theme_bw()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  xlim(0,25)
  #scale_x_continuous(breaks = c(1,2,4,6,10,25))
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/preterm_vs_fullterm.png", height = 3, width = 6, dpi = 600)
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/preterm_vs_fullterm.svg", height = 3, width = 6, dpi = 600)


#different gender twins------------------------------------------------------
twins_diff_gender <- PE_patient %>%
  # Group by mother's ID and delivery time
  group_by(DeID_PatientID, DeID_Delivery_Time) %>%
  # Filter groups with at least 2 babies (twins)
  filter(n() >= 2) %>%
  # Check if there are different genders in the group
  filter(n_distinct(BABY_SEX) > 1) %>%
  # Ungroup to return a clean dataframe
  ungroup()
dim(twins_diff_gender)
