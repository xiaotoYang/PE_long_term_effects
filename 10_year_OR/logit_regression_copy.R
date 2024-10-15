library(tidyverse)
library(pwr)
library(icd)
library(glmnet)
library(car)

# For debug use
library(progress)

load("/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/case_stats_elix_list.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_stats_elix_list.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/case_diag_all_elix_list.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/control_diag_all_elix_list.RData")

all_stats_elix_list = list()
all_final_elix_list = list()
pop_all_list = list()
pop_case_list = list()
pop_control_list = list()

# Combine data and do t-tests
for(i in 1:10){
  case_diag_all_elix_list[[i]] = case_diag_all_elix_list[[i]]%>%
    select(-DeID_AdmitDate.x, -AgeInYears, -start_time.x, -DeID_EncounterID.y, 
           -algorithmName, -TotalScore, -DeID_AdmitDate.y, -start_time.y)
  control_diag_all_elix_list[[i]] = control_diag_all_elix_list[[i]]%>%
    select(-DeID_AdmitDate.x, -AgeInYears, -start_time.x, -DeID_EncounterID.y, 
           -algorithmName, -TotalScore, -DeID_AdmitDate.y, -start_time.y)
}


for (i in 1:10) {
  case_diag_all_elix_list[[i]] = case_diag_all_elix_list[[i]] %>%
    ungroup() %>%
    mutate(across(6:ncol(case_diag_all_elix_list[[i]]), as.double))
  pop_case_list[[i]] = length(unique(case_diag_all_elix_list[[i]]$DeID_PatientID))
  
  control_diag_all_elix_list[[i]] = control_diag_all_elix_list[[i]] %>%
    anti_join(case_diag_all_elix_list[[i]], by = "DeID_PatientID") %>%
    ungroup() %>%
    mutate(across(6:ncol(control_diag_all_elix_list[[i]]), as.double))
  pop_control_list[[i]] = length(unique(control_diag_all_elix_list[[i]]$DeID_PatientID))
  
  all_final_elix_list[[i]] = bind_rows(case_diag_all_elix_list[[i]], control_diag_all_elix_list[[i]])
  pop_all_list[[i]] = length(unique(all_final_elix_list[[i]]$DeID_PatientID))
  
  all_stats_elix_list[[i]] = full_join(case_stats_elix_list[[i]], control_stats_elix_list[[i]], by = "Comorbidity") %>%
    filter(Comorbidity != "HIV") %>%
    group_by(Comorbidity) %>%
    mutate(PropTestP = prop.test(x = c(count.x, count.y), n = c(pop_case_list[[i]], pop_control_list[[i]]), correct = FALSE)$p.value)
}
all_stats_elix_list
save(all_stats_elix_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/all_stats_elix_list.RData")

# Find effective variables and target diseases

target_list = list()
ev_list = list()

for (i in 1:10) {
  # Find the effective variables
  sums = case_diag_all_elix_list[[i]] %>%
    distinct(DeID_PatientID, .keep_all = TRUE) %>%
    select(DeID_PatientID, 7:ncol(case_diag_all_elix_list[[i]])) %>%
    mutate(across(everything(), ~ifelse(is.na(.)|.x == 0, 0, 1))) %>%
    select(-DeID_PatientID) %>%
    colSums()
  na_prop = colSums(is.na(all_final_elix_list[[i]][,7:ncol(all_final_elix_list[[i]])])) / nrow(all_final_elix_list[[i]])
  
  # Remove variables that have more than 20% NA or p > 0,1 in univariable test with PE
  case_como_stats = case_diag_all_elix_list[[i]] %>%
    distinct(DeID_PatientID, .keep_all = TRUE) %>%
    select(AidsHIV:ncol(case_diag_all_elix_list[[i]])) %>%
    colSums(1:ncol(case_diag_all_elix_list[[i]]))
  case_como_stats = data.frame(Comorbidity = names(case_como_stats), case = case_como_stats/pop_case_list[[i]])
  
  control_como_stats = control_diag_all_elix_list[[i]] %>%
    distinct(DeID_PatientID, .keep_all = TRUE) %>%
    select(AidsHIV:ncol(control_diag_all_elix_list[[i]])) %>%
      colSums(1:ncol(control_diag_all_elix_list[[i]]))
  control_como_stats = data.frame(Comorbidity = names(control_como_stats), control = control_como_stats/pop_control_list[[i]])
  
  como_stats = case_como_stats %>%
    left_join(control_como_stats, by = "Comorbidity") %>%
    group_by(Comorbidity) %>%
    mutate(pvalue = prop.test(c(case*pop_case_list[[i]], control*pop_control_list[[i]]), c(pop_case_list[[i]], pop_control_list[[i]]))$p.value)
  
  effective_var = as.array(como_stats$Comorbidity[como_stats$pvalue <= 0.1 & !is.na(como_stats$pvalue)])
  ineffective_var = names(na_prop[na_prop >= 0.2])
  
  all_final_elix_list[[i]] = all_final_elix_list[[i]] %>%
    select(1:6, all_of(setdiff(effective_var, ineffective_var)), "African American":"Other")
  
  # Calculate the effective sample size
  ess = pwr.f2.test(u = ncol(all_final_elix_list[[i]]) - 4, f2 = 0.15, sig.level = 0.05, power = 0.8)$v
  target_list[[i]] = all_stats_elix_list[[i]] %>%
    filter(count.x + count.y >= ess)
  target_list[[i]] = target_list[[i]]$Comorbidity
}

save(target_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/target_list.RData")


# Perform logit regression on confounders: age, PE, gestational diabetes, Elixhauser comobidities, social history, demograhics
result_elix_list = list()
model_list = list()

for (i in 1:10) {
  # For debug use
  pb <-progress_bar$new(
    format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
    total = length(target_list[[i]])
  )
  
  result_elix_list[[i]] = data.frame(Comorbidity = character(),
                                     LogitCoef = double(),
                                     CoefStd = double(),
                                     LogitRegP = double())
  for (disease in target_list[[i]]) {
    disease_data = all_final_elix_list[[i]] %>%
      group_by(DeID_PatientID) %>%
      mutate(had_disease = ifelse(any(Comorbidity == disease), 1, 0)) %>%
      distinct(DeID_PatientID, .keep_all = TRUE) %>%
      ungroup() %>%
      relocate(had_disease, .before = 5) %>%
      select(3:last_col())
    
    #model = glm(unlist(disease_data$had_disease) ~ PE + AgeInYears+SmokingStatusMapped+
    #              IllegalDrugUserStatusMapped+`African American`+Other+Caucasian, 
    #            data = disease_data[,2:ncol(disease_data)], family = binomial(link = "logit"))
    model = glm(unlist(disease_data$had_disease) ~ ., data = disease_data[,2:ncol(disease_data)], family = binomial(link = "logit"))
    
    result_elix_list[[i]] = result_elix_list[[i]] %>% 
      bind_rows(data.frame(Comorbidity = disease,
                           LogitCoef = summary(model)$coefficients["PE", "Estimate"],
                           CoefStd = summary(model)$coefficients["PE", "Std. Error"],
                           LogitRegP = summary(model)$coefficients["PE", "Pr(>|z|)"]
      )
      )
    
    # For debug use
    pb$tick()
  }
  model_list[[i]] = model
}

save(model_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/model_list.RData")

# Replace abbreviations with full names
replacement_rules <- c(
  "HIV" = "Aids HIV",
  "Alcohol" = "Alcohol Abuse",
  "BloodLoss" = "Blood Loss Anemia",
  "Arrhythmia" = "Cardiac Arrhythmias",
  "Pulmonary" = "Chronic Pulmonary Disease",
  "Coagulopathy" = "Coagulopathy",
  "CHF" = "Congestive Heart Failure",
  "Anemia" = "Deficiency Anemia",
  "Depression" = "Depression",
  "DM" = "Diabetes Uncomplicated",
  "DMcx" = "Diabetes Complicated",
  "Drugs" = "Drug Abuse",
  "FluidsLytes" = "Fluid Electrolyte Disorders",
  "HTNcx" = "Hypertension Complicated",
  "HTN" = "Hypertension Uncomplicated",
  "Hypothyroid" = "Hypothyroidism",
  "Liver" = "Liver Disease",
  "Lymphoma" = "Lymphoma",
  "Mets" = "Metastatic Cancer",
  "Obesity" = "Obesity",
  "NeuroOther" = "Other Neurological Disorders",
  "Paralysis" = "Paralysis",
  "PUD" = "Peptic Ulcer Disease Excluding Bleeding",
  "PVD" = "Peripheral Vascular Disorders",
  "Psychoses" = "Psychoses",
  "PHTN" = "Pulmonary Circulation Disorders",
  "Renal" = "Renal Failure",
  "Rheumatic" = "Rheumatoid Arthritis",
  "Tumor" = "Solid Tumor Without Metastasis",
  "Valvular" = "Valvular Disease",
  "WeightLoss" = "Weight Loss"
)

# Find significant results: p-values < 0.05 in both t-test and regression model


for (i in 1:10) {
  result_elix_list[[i]] = all_stats_elix_list[[i]] %>%
    filter(Comorbidity %in% target_list[[i]]) %>%
    left_join(result_elix_list[[i]], by = "Comorbidity") %>%
    mutate(fold = Rate.x / Rate.y) %>%
    mutate(odds = exp(LogitCoef)) %>%
    mutate(omin = exp(LogitCoef - 1.96*CoefStd)) %>%
    mutate(omax = exp(LogitCoef + 1.96*CoefStd))
  
  result_elix_list[[i]]$Comorbidity = replacement_rules[result_elix_list[[i]]$Comorbidity]
  #sign_results_elix_list[[i]] = filter(result_elix_list[[i]], LogitRegP < 0.05)
  
}

sign_results_elix_list = list()
for(i in 1:10){
  sign_results_elix_list[[i]] = result_elix_list[[i]]%>%filter(LogitRegP < 0.05)
}

save(result_elix_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/result_elix_list.RData")
save(sign_results_elix_list, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/sign_results_elix_list.RData")

sign_results_elix_tab = data.frame(sign_results_elix_list[[1]]) %>%
  mutate(Year = 1)
for (i in 2:10) {
  temp = data.frame(sign_results_elix_list[[i]]) %>%
    mutate(Year = i)
  sign_results_elix_tab = sign_results_elix_tab %>%
    rbind(temp) 
}

# Comorbidities that are significant in more than 5 years
x = as.data.frame(table(sign_results_elix_tab$Comorbidity))
print(filter(x, Freq >=3))

# varaible truth table
for (i in 1:10) {ev_list[[i]] = colnames(all_final_elix_list[[i]])}
max_length <- max(sapply(ev_list, length))

# Pad shorter arrays with NA to make them equal in length
padded_list <- lapply(ev_list, function(x) c(x, rep(NA, max_length - length(x))))

# Convert the list to a matrix
word_matrix <- matrix(unlist(padded_list), nrow = length(ev_list), byrow = TRUE)

# Convert the matrix to a data frame for easier handling
word_table <- as.data.frame(t(word_matrix))
write.csv(word_table, file = "/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/word_table.csv", row.names = TRUE)

