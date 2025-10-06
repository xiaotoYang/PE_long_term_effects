library("survival")
library("survminer")
library(dplyr)

load("/nfs/turbo/umms-lgarmire2/Xiaotong/case_survival_list_XY_200dcomo.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/control_survival_list_XY_200dcomo.RData")


#All Patients-----------------------------------------------------------------------------------------
#KM Curves
target = c("HTN", "Renal", "DM","DMcx","Obesity")
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




# by race---------------------------------------------------
target = c("HTN", "Renal")
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


