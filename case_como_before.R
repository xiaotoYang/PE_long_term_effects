library(tidyverse)
library(touch)
library(icd)

setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/case_como_before/")

diag = read.csv("DiagnosesComprehensiveAll.csv")
encounter = read.csv("EncounterAll.csv")

load("/nfs/turbo/umms-lgarmire2/Xiaotong/case_diag_date.RData")

diag$Lexicon <- ifelse(grepl("^[0-9]", diag$TermCodeMapped), "icd 9", "icd 10")
diag[diag$Lexicon == "icd 9","TermCodeMapped"] = icd_map(diag[diag$Lexicon == "icd 9","TermCodeMapped"])
diag$TermCodeMapped = gsub(",.*", "", diag$TermCodeMapped)
diag$TermCodeMapped[diag$Lexicon == "icd 9"] = short_to_decimal(diag$TermCodeMapped[diag$Lexicon == "icd 9"])

diag_date = diag_date%>%group_by(DeID_PatientID)%>%slice(1)%>%select(DeID_PatientID, start_time)
diag = diag%>%left_join(diag_date)
diag = diag %>%
  left_join(encounter, by = c("DeID_PatientID", "DeID_EncounterID")) %>%
  filter(!is.na(DeID_AdmitDate))
diag$DeID_AdmitDate = as.Date(diag$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
save(diag, file = "case_como_before_diag.RData")

diag_90d_before = diag%>%filter(DeID_AdmitDate<start_time-90) #pre-existing condition
length(unique(diag_90d_before$DeID_PatientID))
n_diag = diag_90d_before%>%group_by(DeID_PatientID)%>%summarise(n=n())
quantile(n_diag$n)

test <- diag_90d_before %>%
  icd10_comorbid_elix() %>%
  as.data.frame() %>%
  mutate(DeID_EncounterID = rownames(.))%>%
  group_by(DeID_PatientID)%>%
  summarise()

como = read.csv("ComorbiditiesElixhauserComprehensive.csv")
como = como%>%inner_join(diag_date)%>%left_join(encounter)
como$DeID_AdmitDate = as.Date(como$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
como_earliest = como%>%
  filter(AidsHIV!=-99)%>%
  group_by(DeID_PatientID) %>%
  filter(DeID_AdmitDate <= start_time-200) %>% #try pre-pregnancy comorbidities
  mutate(across(AidsHIV:WeightLoss, ~max(.))) %>%
  arrange(desc(DeID_AdmitDate), by_group = TRUE)%>%
  slice(1) 

save(como_earliest, file= "case_como_200dbefore.RData")

