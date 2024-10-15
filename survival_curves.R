library("survival")
library("survminer")
library(dplyr)

load("/nfs/turbo/umms-lgarmire2/Xiaotong/case_survival_list_XY_200dcomo.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/control_survival_list_XY_200dcomo.RData")


#All Patients-----------------------------------------------------------------------------------------
#KM Curves
target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  data1 = data1%>%filter(surv_time<=3650)%>%mutate(surv_time = surv_time/365)
  data1$PE = ifelse(data1$PE==1, "Case", "Control")
  fit<- survfit(Surv(surv_time, status) ~ PE, data = data1)
  p= survdiff(Surv(surv_time, status) ~ PE, data = data1)
  f = ggsurvplot(fit, pval = TRUE, conf.int = FALSE,
                 ylim = c(max(min(fit$lower)-0.05, 0), 1),
                 pval.coord =c(0, min(fit$lower)-0.05),
                 legend.title = "Group", legend.labs = c("Case", "Control"), palette=c("#2E9FDF", "#E7B800"),
                 title = paste0("Survival Curve of ", target[i]), xlab = "Years", ylab = "Survival Probability")
  f$plot <- f$plot + 
    #annotate("text", x = 1, y = c(0, min(fit$lower)-0.05), label = paste("p =", format(round(p$pvalue,4), scientific = TRUE)), size = 5)+
    theme(legend.text = element_text(size = 14, color = "black"), legend.title = element_text(size = 14))
  png(height = 3000, width = 3000, file = paste0("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/Survival_", target[i], "_1014.png"), res = 600)
  print(f)
  dev.off()
}



#coxph and hazard ratio
library(dplyr)
result_survival = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  fit = coxph(Surv(surv_time, status) ~ PE+PE_age+Caucasian+SmokingStatusMapped+
                AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated + 
                Hypothyroidism + Obesity + RenalFailure + CongestiveHeartFailure, data = data1)
  result_survival = result_survival %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         coef = summary(fit)$coefficients["PE", "coef"],
                         stderr = summary(fit)$coefficients["PE", "se(coef)"],
                         pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_survival
write.csv(result_survival, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/result_survival_new.csv")


figure = ggplot(data=result_survival, aes(y= Comorbidity, x=exp(coef), xmin=exp(coef-1.96*stderr), xmax=exp(coef+1.96*stderr))) +
  theme_minimal()+
  geom_errorbarh(height=.1)+
  geom_point(color = "red") + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Hazard Ratios of PE", x = "Hazard Ratio", y = "Comorbidity") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(1,2,4,6))
figure
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/HazardRatio_new.png", height = 4.8, width = 7.2, dpi = 600)



# by race---------------------------------------------------
i=1
for (disease in target) {
  sdata = rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  sdata = sdata%>%filter(surv_time<=3650)%>%mutate(surv_time = surv_time/365)
  sdata = sdata%>%
    filter(Caucasian == 1 | `African American` == 1) %>%
    mutate(Race = ifelse(Caucasian == 1, "Caucasian", "African American")) %>%
    mutate(Group = ifelse(PE == 1, "Case", "Control"))
  sdata = as.data.frame(sdata)
  sfit = survfit(Surv(surv_time, ifelse(status == 1, 1, 0), type = "right")~Group+Race, data = sdata)
  
  temp = ggsurvplot(sfit, sdata, pval = TRUE,
                    conf.int=FALSE,
                    ylim = c(max(min(sfit$lower)-0.05, 0), 1),
                    pval.coord =c(0, min(sfit$lower)-0.05),
                    legend.title = "Race", 
                    title = paste0("Survival Curve of ", disease), xlab = "Years", ylab = "Survival Probability",
                    # palette = c("#D0717C","#6DA6CF"), 
                    font.tickslab = c(12),
                    facet.by = "Race"
  )
  temp$plot <- temp$plot + 
    theme(legend.text = element_text(size = 15, color = "black"), legend.title = element_text(size = 15),
          strip.text.x = element_text(size = 15))
  png(height = 3000, width = 4000, file = paste0("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/Survival_race_", disease, "1014.png"), res = 600)
  print(temp)
  dev.off()
  i = i + 1
}




# result_survival_race = data.frame()
# for(i in c(1:length(target))){
#   data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
#   fit = coxph(Surv(surv_time, status) ~ PE+PE_age+Caucasian+ PE:Caucasian+SmokingStatusMapped+AlcoholUseStatusMapped, data = data1)
#   result_survival_race = result_survival_race %>% 
#     bind_rows(data.frame(Comorbidity = target[i],
#                          coef = summary(fit)$coefficients["PE:Caucasian", "coef"],
#                          stderr = summary(fit)$coefficients["PE:Caucasian", "se(coef)"],
#                          pvalue = summary(fit)$coefficients["PE:Caucasian", "Pr(>|z|)"]))
# }
# result_survival_race
# write.csv(result_survival_race, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/result_survival_race_interaction_term.csv")

#caucasian
result_survival_caucasian = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  data1 = data1%>%filter(Caucasian==1)
  fit = coxph(Surv(surv_time, status) ~ PE+PE_age+SmokingStatusMapped+AlcoholUseStatusMapped, data = data1)
  result_survival_caucasian  = result_survival_caucasian  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         coef = summary(fit)$coefficients["PE", "coef"],
                         stderr = summary(fit)$coefficients["PE", "se(coef)"],
                         pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_survival_caucasian 

result_survival_AA = data.frame()
for(i in c(1:length(target))){
  data1 <- rbind(case_survival_list[[target[i]]], control_survival_list[[target[i]]])
  data1 = data1%>%filter(`African American`==1)
  fit = coxph(Surv(surv_time, status) ~ PE+PE_age+SmokingStatusMapped+AlcoholUseStatusMapped, data = data1)
  result_survival_AA  = result_survival_AA  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         coef = summary(fit)$coefficients["PE", "coef"],
                         stderr = summary(fit)$coefficients["PE", "se(coef)"],
                         pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_survival_AA

data = as.data.frame(rbind(result_survival_caucasian,result_survival_AA))
data$race = c(rep("Caucasian", 6), rep("AA", 6))
data
figure2 = ggplot(data=data, aes(y= Comorbidity, x=exp(coef), xmin=exp(coef-1.96*stderr), xmax=exp(coef+1.96*stderr), color = race, group = race)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Hazard Ratios of PE", x = "Hazard Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure2

ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/HazardRatio_race.png", height = 4.8, width = 7.2, dpi = 600)

#3. by severity------------------------------------------------------
library(icd)
library(touch)
library(tidyverse)

diag = read.csv("/nfs/turbo/umms-lgarmire2/haomingz-copy/long_term_effect_PE_3/case/DiagnosesComprehensiveAll.csv")
encounter = read.csv("/nfs/turbo/umms-lgarmire2/haomingz-copy/long_term_effect_PE_3/case/EncounterAll.csv")
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

#survival curves
target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
i=1
for (disease in target) {
  severe = case_survival_list[[target[i]]]%>%inner_join(severe_pe)%>%mutate(group = "severe")
  mild = case_survival_list[[target[i]]]%>%inner_join(mild_pe)%>%mutate(group = "mild")
  control = control_survival_list[[target[i]]]%>%mutate(group = "control")
  sdata = rbind(severe, mild, control)
  sdata = as.data.frame(sdata)
  sdata = sdata%>%filter(surv_time<=3650)%>%mutate(surv_time = surv_time/365)
  sfit = survfit(Surv(surv_time, ifelse(status == 1, 1, 0), type = "right")~group, data = sdata)
  
  temp = ggsurvplot(sfit, sdata, pval = TRUE,
                    ylim = c(max(min(sfit$lower)-0.05, 0), 1),
                    pval.coord =c(0, min(sfit$lower)-0.05),
                    conf.int=FALSE,
                    legend.title = "Group", 
                    legend.labs = c("control", "mild", "severe"),
                    title = paste0("Survival Curve of ", disease), xlab = "Years", ylab = "Survival Probability",
                    # palette = c("#D0717C","#6DA6CF"), 
                    font.tickslab = c(12))
  temp$plot <- temp$plot + 
    theme(legend.text = element_text(size = 14, color = "black"), legend.title = element_text(size = 14))
  
  png(height = 3000, width = 3000, file = paste0("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/Survival_severity_", disease, "1014.png"), res = 600)
  print(temp)
  dev.off()
  i = i + 1
}


#hazard ratio
result_survival_severe = data.frame()
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(severe_pe)
  data1 <- rbind(case, control_survival_list[[target[i]]])
  fit = coxph(Surv(surv_time, status) ~ PE+PE_age+Caucasian+SmokingStatusMapped+
                AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated + 
                Hypothyroidism + Obesity + RenalFailure + CongestiveHeartFailure, data = data1)
  result_survival_severe  = result_survival_severe  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         coef = summary(fit)$coefficients["PE", "coef"],
                         stderr = summary(fit)$coefficients["PE", "se(coef)"],
                         pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_survival_severe 

result_survival_mild = data.frame()
for(i in c(1:length(target))){
  case = case_survival_list[[target[i]]]%>%inner_join(mild_pe)
  data1 <- rbind(case, control_survival_list[[target[i]]])
  fit = coxph(Surv(surv_time, status) ~ PE+PE_age+Caucasian+SmokingStatusMapped+
                AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated + 
                Hypothyroidism + Obesity + RenalFailure + CongestiveHeartFailure, data = data1)
  result_survival_mild  = result_survival_mild  %>% 
    bind_rows(data.frame(Comorbidity = target[i],
                         coef = summary(fit)$coefficients["PE", "coef"],
                         stderr = summary(fit)$coefficients["PE", "se(coef)"],
                         pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
}
result_survival_mild

data = as.data.frame(rbind(result_survival_severe,result_survival_mild))
data$Severity = c(rep("severe", 6), rep("mild", 6))
data
figure3 = ggplot(data=data, aes(y= Comorbidity, x=exp(coef), xmin=exp(coef-1.96*stderr), xmax=exp(coef+1.96*stderr), color = Severity, group = Severity)) +
  theme_minimal()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5))+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(title = "Hazard Ratios of PE", x = "Hazard Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6)) 
figure3

ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/HazardRatio_severity.png", height = 4.8, width = 7.2, dpi = 600)


#recurrent pe--------------------------------------------------------
# check how many patients have multiple pe
# all_pe = diag_convert %>%
#   filter(grepl("O14", TermCodeMapped))%>%
#   filter(!grepl("O14.2", TermCodeMapped)) 
# all_pe = all_pe%>%left_join(encounter)
# tbl = all_pe%>%select(DeID_PatientID, AgeInYears)%>%group_by(DeID_PatientID)%>%summarise(earliest = min(AgeInYears))
# tbl2 = all_pe%>%left_join(tbl)%>%mutate(diff = AgeInYears-earliest)%>%group_by(DeID_PatientID)%>%summarise(multi = diff>1)
# tbl2 = tbl2%>%group_by(DeID_PatientID)%>%summarise(multiple_pe = any(multi == TRUE))
# recurrent_pe = tbl2%>%filter(multiple_pe == TRUE)%>%select(DeID_PatientID)
# 
# final_recurrent_pe = case_survival_list$HTN%>%inner_join(recurrent_pe)#592 patients
# 
# #hazard ratio
# result_survival_recurrent = data.frame()
# i=1
# for(i in c(1:length(target))){
#   case = case_survival_list[[target[i]]]%>%inner_join(recurrent_pe)
#   data1 <- rbind(case, control_survival_list[[target[i]]])
#   fit = coxph(Surv(surv_time, status) ~ PE+PE_age+Caucasian+SmokingStatusMapped+
#                 AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated + 
#                 Hypothyroidism + Obesity + RenalFailure+DiabetesUncomplicated, data = data1)
#   result_survival_recurrent  = result_survival_recurrent  %>% 
#     bind_rows(data.frame(Comorbidity = target[i],
#                          coef = summary(fit)$coefficients["PE", "coef"],
#                          stderr = summary(fit)$coefficients["PE", "se(coef)"],
#                          pvalue = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
# }
# result_survival_recurrent
