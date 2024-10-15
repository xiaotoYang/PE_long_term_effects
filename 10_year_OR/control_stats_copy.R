library(dplyr)
library(tidyr)
library(touch)
library(lubridate)
library(icd)


# diag = read.csv("DiagnosesComprehensiveAll.csv")
# encounter = read.csv("EncounterAll.csv")
# como = read.csv("ComorbiditiesElixhauserComprehensive.csv")
race = read.csv("/nfs/turbo/umms-lgarmire2/haomingz-copy/long_term_effect_PE_3/control/PatientRace.csv")
social = read.csv("/nfs/turbo/umms-lgarmire2/haomingz-copy/long_term_effect_PE_3/control/ClaritySocialHistory.csv")

# diag_convert = diag
# 
# diag_convert[diag_convert$Lexicon == "ICD9","TermCodeMapped"] = icd_map(diag_convert[diag_convert$Lexicon == "ICD9","TermCodeMapped"])
# diag_convert$TermCodeMapped = gsub(",.*", "", diag_convert$TermCodeMapped)
# diag_convert$TermCodeMapped[diag_convert$Lexicon == "ICD9"] = short_to_decimal(diag_convert$TermCodeMapped[diag_convert$Lexicon == "ICD9"])
# 
# # Match the encounter date with diagnoses
# # Add a new column "PE" and set it to 0
# diag_date = diag_convert %>%
#   select(-TermNameMapped, -Lexicon) %>%
#   left_join(encounter, by = c("DeID_PatientID", "DeID_EncounterID")) %>%
#   filter(!is.na(DeID_AdmitDate)) %>%
#   mutate(PE = 0) 
# 
# diag_date$DeID_AdmitDate = as.Date(diag_date$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
# 
# como_earliest = diag_date %>%
#   right_join(como, by = c("DeID_PatientID", "DeID_EncounterID")) %>%
#   filter(grepl("Z34", TermCodeMapped)) %>%
#   group_by(DeID_PatientID) %>%
#   arrange(DeID_AdmitDate) %>%
#   slice(1) %>%
#   select(-DeID_EncounterID, -TermCodeMapped, -DeID_AdmitDate, -AgeInYears, -PE)
# 
# diag_date = diag_date %>%
#   group_by(DeID_PatientID) %>%
#   mutate(GestationalDiabetes = if_else(any(grepl("O24", TermCodeMapped)) & DeID_AdmitDate > min(DeID_AdmitDate[grepl("O24", TermCodeMapped)]), 1, 0))

# Filter diagnoses occurring 1~10 years after first PE

diag_date_list = list()
pop_control_list = list()

for (i in 1:10) {
  diag_date_i = diag_date %>%
    group_by(DeID_PatientID) %>%
    filter(max(DeID_AdmitDate) >= (start_time + years(i))) %>% #follow-up time is longer than current year
    filter(DeID_AdmitDate >= start_time+90) %>% #select diagnosis happened within the current year
    filter(DeID_AdmitDate < (start_time + years(i)))
  pop_control_i = length(unique(diag_date_i$DeID_PatientID))
  diag_date_list[[i]] = diag_date_i
  pop_control_list[[i]] = pop_control_i
}

diag_elix_list = list()

for (i in 1:10) {
  diag_elix_i = as.data.frame(icd10_comorbid_elix(diag_date_list[[i]]))
  diag_elix_i = diag_elix_i %>%
    mutate(DeID_EncounterID = rownames(diag_elix_i)) %>%
    gather(key = "key", value = "value", -DeID_EncounterID) %>%
    filter(value) %>%
    group_by(DeID_EncounterID) %>%
    summarise(Comorbidity = list(key)) %>%
    ungroup()
  new_data = diag_date_list[[i]] %>% 
    left_join(diag_elix_i, by = "DeID_EncounterID") %>%
    unnest(Comorbidity) %>%
    select(-TermCodeMapped) %>%
    group_by(DeID_PatientID) %>%
    arrange(DeID_AdmitDate) %>%
    distinct(Comorbidity, .keep_all = TRUE) %>%
    relocate(Comorbidity, .before = "AgeInYears")
  diag_elix_list[[i]] = new_data
}


# preterm_hist = pregnancy %>%
#   select(DeID_PatientID, OUTCOME) %>%
#   mutate(Preterm = ifelse(OUTCOME == "PRETERM", 1, 0)) %>%
#   select(-OUTCOME) %>%
#   group_by(DeID_PatientID) %>%
#   slice_max(Preterm) %>%
#   distinct(DeID_PatientID, .keep_all = TRUE)

# Remove repeated diagnoses in one patient, categorize by first 3 ICD codes

diag_hist_list = list()

social_hist = social %>%
  select(-TobaccoPacksPerDay, -TobaccoUsedYears) %>%
  mutate(SmokingStatusMapped = ifelse(SmokingStatusMapped %in% c("Current", "Former"), 1, 0)) %>%
  mutate(AlcoholUseStatusMapped = ifelse(AlcoholUseStatusMapped == "Yes", 1, 0)) %>%
  mutate(IllegalDrugUserStatusMapped = ifelse(IllegalDrugUserStatusMapped == "Yes", 1, 0)) %>%
  mutate(SexuallyActiveStatusMapped = ifelse(SexuallyActiveStatusMapped == "Yes", 1, 0)) %>%
  mutate(CigarettesYN = ifelse(CigarettesYN == "Y", 1, 0)) %>%
  mutate(PipesYN = ifelse(PipesYN == "Y", 1, 0)) %>%
  mutate(CigarsYN = ifelse(CigarsYN == "Y", 1, 0)) %>%
  mutate(SnuffYN = ifelse(SnuffYN == "Y", 1, 0)) %>%
  mutate(ChewYN = ifelse(ChewYN == "Y", 1, 0)) %>%
  mutate(IVDrugUserYN = ifelse(IVDrugUserYN == "Y", 1, 0)) %>%
  group_by(DeID_PatientID) %>%
  mutate(across(SmokingStatusMapped:IVDrugUserYN, ~ifelse(any(.x == 1), 1, 0 ))) %>%
  distinct(DeID_PatientID, .keep_all = TRUE) %>%
  select(-DeID_EncounterID)

race_hist = race %>%
  mutate(RaceName = if_else(!(RaceName %in% c("Caucasian", "African American")), "Other", RaceName)) %>%
  mutate(value = 1) %>%
  spread(key = RaceName, value = value, fill = 0) %>%
  select(-Line, -RaceCode) %>%
  distinct(DeID_PatientID, .keep_all = TRUE)


for (i in 1:10) {
  diag_hist_i = diag_date_list[[i]] %>%
    group_by(DeID_PatientID) %>%
    arrange(desc(DeID_AdmitDate)) %>%
    distinct(TermCodeMapped, .keep_all = TRUE)
  diag_hist_list[[i]] = diag_hist_i
}


# Add comorbidities, social hist, race

control_diag_all_list = list()
control_diag_all_elix_list = list()

for (i in 1:10) {
  diag_all_i = diag_hist_list[[i]] %>% 
    inner_join(como_earliest, by = "DeID_PatientID") %>%
    left_join(social_hist, by = "DeID_PatientID") %>%
    left_join(race_hist, by = "DeID_PatientID")
  
  diag_all_elix_i = diag_elix_list[[i]] %>% 
    inner_join(como_earliest, by = "DeID_PatientID") %>%
    left_join(social_hist, by = "DeID_PatientID") %>%
    left_join(race_hist, by = "DeID_PatientID")
  
  control_diag_all_list[[i]] = diag_all_i
  control_diag_all_elix_list[[i]] = diag_all_elix_i
}

save(control_diag_all_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_diag_all_list.RData")
save(control_diag_all_elix_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_diag_all_elix_list.RData")

control_stats_list = list()
control_stats_elix_list = list()

for (i in 1:10) {
  control_stats_i = diag_hist_list[[i]] %>%
    group_by(TermCodeMapped) %>%
    summarise(count = n()) %>%
    mutate(Rate = count / pop_control_list[[i]])
  control_stats_list[[i]] = control_stats_i
  
  control_stats_elix_i = diag_elix_list[[i]] %>%
    group_by(Comorbidity) %>%
    summarise(count = n()) %>%
    mutate(Rate = count / pop_control_list[[i]])
  control_stats_elix_list[[i]] = control_stats_elix_i
}

save(control_stats_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_stats_list.RData")
save(control_stats_elix_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_stats_elix_list.RData")

