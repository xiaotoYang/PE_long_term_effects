# odds ratio all
setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables")

load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/case_list_for_or.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/control_list_for_or.RData")

library(dplyr)
target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
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
                Hypothyroidism + Obesity + RenalFailure + DiabetesUncomplicated,  family = binomial(link = "logit"), data = data1)
  result = result %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}


result
write.csv(result, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result.csv")

###hypothyroid 4 years
i = 6
data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
for(j in c(1:nrow(data1))){
  if(data1$surv_time[j]>365*10){
    data1$status[j] = 0
  }
}
fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
            AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
            Hypothyroidism + Obesity + RenalFailure + DiabetesUncomplicated + Caucasian,  family = binomial(link = "logit"), data = data1)
result = result%>%
  bind_rows(data.frame(Comorbidity = target[i],
                       LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                       CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                       LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
result
###hypothyroid ends---------------------------------------------------

library(ggplot2)
fig = ggplot(data=result, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), xmax=exp(LogitCoef+1.96*CoefStd))) +
  theme_minimal()+
  geom_errorbarh(height=.1)+
  geom_point(color = "red") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Complication")
fig
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/all_odds_ratio_UM.png", height = 4.8, width = 7.2, dpi = 600)
#png(result_survival, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/all_odds_ratio_UM.png")


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
              Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
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

# recurrent pe ------------------------------------------
# check how many patients have multiple pe

recurrent_pe = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/recurrent_pe_list.csv")
final_recurrent_pe = case_survival_list$HTN%>%inner_join(recurrent_pe)#692 patients
multiparous_control = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/mulitparous.csv")
final_multiparous_control = control_survival_list$HTN%>%inner_join(multiparous_control)#9765
one_pe = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/one_pe_list.csv")#885
colnames(one_pe) = c("X", "DeID_PatientID")
final_one_pe = case_survival_list$HTN%>%inner_join(one_pe)
final_multi = case_survival_list$HTN%>%inner_join(multiparous)

result_recurrent = data.frame()
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(recurrent_pe)
  control = control_survival_list[[target[i]]]%>%inner_join(multiparous_control)
  data1 <- rbind(case, control)
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure + DiabetesUncomplicated+Caucasian,  family = binomial(link = "logit"), data = data1)
  result_recurrent  = result_recurrent %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}

result_nonrecurrent = data.frame()
i=1
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(one_pe)
  control = control_survival_list[[target[i]]]%>%inner_join(multiparous_control)
  data1 <- rbind(case, control_survival_list[[target[i]]])
  for(j in c(1:length(data1))){
    if(data1$surv_time[j]>3650){
      data1$status[j] = 0
    }
  }
  fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
              AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
              Hypothyroidism + Obesity + RenalFailure + DiabetesUncomplicated+Caucasian,  family = binomial(link = "logit"), data = data1)
  result_nonrecurrent  = result_nonrecurrent %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                         CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                         LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}

result_nonrecurrent
data = as.data.frame(rbind(result_recurrent,result_nonrecurrent))
data$recurrent = c(rep("Recurrent PE", 6), rep("Multiparous, one PE", 6))
data
write.csv(data, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/recurrent_pe_result.csv")

library(ggplot2)
figure4 = ggplot(data=data, aes(y=Comorbidity, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), xmax=exp(LogitCoef+1.96*CoefStd), group = recurrent, color = recurrent)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure4
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/OR_recurrent.png", height = 4.8, width = 7.2, dpi = 600)




