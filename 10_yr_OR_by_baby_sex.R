#Baby sex 10 years OR

#load previous OR data
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/case_list_for_or.RData")
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/control_list_for_or.RData")
#load baby info
baby_case = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/Stork_Pregnancy_Outcomes_case.csv")
baby_control = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/baby_sex_and_sga/Stork_Pregnancy_Outcomes_control.csv")


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

#remove different-sex times
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

#10-year OR


#function to calculate x-year OR given baby sex and time_frame(x)
#baby_sex: either "MALE" or "FEMALE"
#time frame: days
time_frame = c(365, 365*2, 365*3, 365*4, 365*5, 365*6,365*7,365*8, 365*9, 365*10)
calculate_OR = function(baby_sex, disease){
  result = data.frame()
  
  for(i in c(1:length(time_frame))){
    data1 <- rbind(case_survival_list[[disease]]%>%inner_join(PE_no_twins%>%filter(BABY_SEX==baby_sex)%>%select(DeID_PatientID)), 
                   control_survival_list[[disease]]%>%inner_join(control_no_twins%>%filter(BABY_SEX==baby_sex)%>%select(DeID_PatientID)))
    for(j in c(1:nrow(data1))){
      if(data1$surv_time[j]>time_frame[i]){
        data1$status[j] = 0
      }
    }
    fit = glm(status ~ PE+PE_age+Caucasian+SmokingStatusMapped+
                AlcoholUseStatusMapped+DiabetesComplicated + HypertensionUncomplicated +
                Hypothyroidism + Obesity + RenalFailure+ DiabetesUncomplicated ,  family = binomial(link = "logit"), data = data1)
    result  = result%>% 
      bind_rows(data.frame(year = i,
                           LogitCoef = summary(fit)$coefficients["PE", "Estimate"],
                           CoefStd = summary(fit)$coefficients["PE", "Std. Error"],
                           LogitRegP = summary(fit)$coefficients["PE", "Pr(>|z|)"]))
  }
  return(result) 
}


result_female_baby = list()
result_male_baby = list()
target = c("HTN", "DMcx", "DM", "Renal", "Obesity", "Hypothyroid")
for(i in c(1:length(target))){
  result_female_baby[[i]] = calculate_OR("FEMALE",target[i])
  result_male_baby[[i]] = calculate_OR("MALE", target[i])
}
result_female_baby[[1]]
result_female_baby[[2]]

#plot-----------------------------------------------
library(dplyr)
library(ggplot2)

plot_or_ci <- function(female_df, male_df, disease_name) {
  # Add sex labels
  female_df$Sex <- "Female"
  male_df$Sex <- "Male"
  
  # Combine data
  df <- rbind(female_df, male_df)
  df$Year <- rep(1:10, times = 2)  # Assuming years 1 to 10
  
  # Compute 95% Confidence Intervals
  df$Lower_CI <- exp(df$LogitCoef-1.96 * df$CoefStd)
  df$Upper_CI <- exp(df$LogitCoef+1.96 * df$CoefStd)
  
  # Plot
  ggplot(df, aes(x = Year, y = exp(LogitCoef), color = Sex)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.5) +
    labs(title = paste("Odds Ratio Over Time for", disease_name),
         x = "Year",
         y = "Odds Ratio") +
    theme_minimal() +
    theme(legend.position = "top") 
}

plots <- lapply(1:6, function(i) {
  plot_or_ci(result_female_baby[[i]], result_male_baby[[i]], target[i])
})

# Print all plots
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 3))

