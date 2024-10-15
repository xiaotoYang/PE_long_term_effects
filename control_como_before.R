library(tidyverse)
library(touch)
library(icd)

setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/control_como_before/")

encounter = read.csv("EncounterAll.csv")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/control_diag_date.RData")

diag_date = diag_date%>%group_by(DeID_PatientID)%>%slice(1)%>%select(DeID_PatientID, start_time)

como = read.csv("ComorbiditiesElixhauserComprehensive.csv")
como = como%>%left_join(encounter)
como$DeID_AdmitDate = as.Date(como$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
como = como%>%inner_join(diag_date, by= c("DeID_PatientID"))
como_earliest = como%>%
  filter(AidsHIV!=-99)%>%
  group_by(DeID_PatientID) %>%
  filter(DeID_AdmitDate <= start_time-200) %>% #try pre-pregnancy comorbidities
  mutate(across(AidsHIV:WeightLoss, ~max(.))) %>%
  arrange(desc(DeID_AdmitDate), by_group = TRUE)%>%
  slice(1) 

control_como_before_summary = as.data.frame(colSums(como_earliest[4:34]))
names(control_como_before_summary) = "numbers_of_patients"
write.csv(control_como_before_summary, "control_como_before_summary.csv")
save(como_earliest, file= "control_como_200dbefore.RData")


