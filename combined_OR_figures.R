library(ggplot2)
library(dplyr)
setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables")


# load 4 datasets
load("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/ukbb_valid_result.RData")
result_list = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/cedars_sinai/result_list.csv")
UM_result <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result.csv")
vanderbilt = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/vanderbilt/result_Feb25.csv")


result_list$Comorbidity = c("hypertension_uncomplicated",  "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
UM_result$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                          "diabetes_uncomplicated", "renal_failure","obesity")
valid_result$Comorbidity = c("hypertension_uncomplicated",  "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
vanderbilt$Comorbidity = c("hypertension_uncomplicated","diabetes_uncomplicated", "diabetes_complicated","renal_failure", "obesity")

result_list = result_list[,2:5]
UM_result = UM_result[,2:5]
names(valid_result) = names(UM_result)
data = as.data.frame(rbind(UM_result, result_list,valid_result, vanderbilt))
data$institute = c(rep("UMich", 5),rep("Cedars-Sinai", 5),  rep("UKBiobank", 5), rep("Vanderbilt", 5))
data$institute = factor(data$institute, levels = c("Cedars-Sinai", "UKBiobank", "Vanderbilt","UMich"))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","diabetes_complicated","renal_failure", "diabetes_uncomplicated","obesity"))
data = data%>%filter(!Comorbidity == "hypothyroidism")

figure = ggplot(data=data, aes(y=institute, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), 
                               xmax=exp(LogitCoef+1.96*CoefStd), color = institute)) +
  theme_minimal()+
  guides(color="none")+
  geom_errorbarh(height=0.3, position = position_dodge(width = 0.7), linewidth = 1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20),
    labels = c("1", "2", "5", "10", "20")
  ) +
  facet_grid(Comorbidity~.)+
  theme_bw()+
  theme(strip.text.y= element_text(angle = 0),axis.title = element_text(size = 13), axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text = element_text(size = 13), strip.text = element_text(size = 13))
figure

ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/combine_odds_0521.png", height = 4, width = 8, dpi = 600)


#race-specific result------------------------------------------------------------------
result_race = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/cedars_sinai/result_race.csv")
result_race = result_race[,2:5]
UM_result_white <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result_caucasian.csv")
UM_result_black <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result_AA.csv")
vanderbilt_white = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/vanderbilt/result2_Caucasian.csv")
vanderbilt_black = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/vanderbilt/result2_AA.csv")
UM_result_white = UM_result_white[,2:5]
UM_result_white$Comorbidity = c("hypertension_uncomplicated",  "renal_failure")

UM_result_black = UM_result_black[,2:5]
UM_result_black$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
vanderbilt_black$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
vanderbilt_white$Comorbidity = c("hypertension_uncomplicated","renal_failure")
result_race$Comorbidity = rep(c("hypertension_uncomplicated",  "renal_failure"),3)

data = as.data.frame(rbind(UM_result_white, vanderbilt_white, UM_result_black, vanderbilt_black, result_race))
data$race = c(rep("Caucasian", 4), rep("African American", 4), rep("Caucasian",2), rep("African American", 2), rep("Asian", 2))

data$institute = c(rep("UMich", 2), rep("Vanderbilt", 2), rep("UMich", 2),rep("Vanderbilt", 2), rep("Cedars-Sinai", 6))
data$institute = factor(data$institute, levels = c("Cedars-Sinai",  "Vanderbilt", "UMich"))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","renal_failure"))
data = data%>%filter(!(institute=="Cedars-Sinai"&Comorbidity=="renal_failure"))

figure2 = ggplot(data=data, aes(y=institute, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = race, group = race)) +
  theme_bw()+
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(Comorbidity~.)+
  theme_bw()+
  theme(strip.text.y= element_text(angle = 0),axis.title = element_text(size = 13), axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text = element_text(size = 13), strip.text = element_text(size = 13), legend.text=element_text(size=13))
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/combine_odds_race0516.png", height = 4, width = 12, dpi = 600)


### PE by severity---------------------------------
UM_result_mild <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result_mild.csv")
UM_result_severe <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/UM_result_severe.csv")
vanderbilt_mild = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/vanderbilt/result2_mildPE.csv")
vanderbilt_severe = read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/logit_tables/vanderbilt/result2_severePE.csv")


UM_result_mild = UM_result_mild[,2:5]
UM_result_mild$Comorbidity = c("hypertension_uncomplicated",  "renal_failure")
UM_result_severe = UM_result_severe[,2:5]
UM_result_severe$Comorbidity = c("hypertension_uncomplicated","renal_failure")
vanderbilt_mild$Comorbidity = c("hypertension_uncomplicated","renal_failure")
vanderbilt_severe$Comorbidity = c("hypertension_uncomplicated", "renal_failure")

data = as.data.frame(rbind(UM_result_mild, vanderbilt_mild, UM_result_severe, vanderbilt_severe))
data$severity = c(rep("mild", 2), rep("severe", 2))
data$institute = c( rep("UMich", 2), rep("Vanderbilt",2), rep("UMich", 2),  rep("Vanderbilt",2))
data$institute = factor(data$institute, levels = c("Vanderbilt", "UMich"))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","renal_failure"))

figure4 = ggplot(data=data, aes(x=exp(LogitCoef), y = institute, xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = severity, group = severity)) +
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme_bw()+
  theme(strip.text.y= element_text(angle = 0),axis.title = element_text(size = 13), axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text = element_text(size = 13), strip.text = element_text(size = 13), legend.text=element_text(size=13))

figure4
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/combine_odds_severity0516.png", height = 4, width = 10, dpi = 600)


#baby sex-----------------------------------------------
#all
um_male = read.csv("baby_sex_and_sga/result_male_baby.csv")
um_female = read.csv("baby_sex_and_sga/result_female_baby.csv")
vanderbilt_male = read.csv("logit_tables/result_male_baby_Feb25.csv")
vanderbilt_female = read.csv("logit_tables/result_female_baby_Feb25.csv")
cedar_sinai_male = read.csv("logit_tables/cedars_sinai/cedar_sinai_male_infant.csv")
cedar_sinai_female = read.csv("logit_tables/cedars_sinai/cedar_sinai_female_infant.csv")

um_male$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
um_female$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
data = as.data.frame(rbind(um_male, um_female))
data$infant_sex = c(rep("Male", 2), rep("Female", 2))

vanderbilt_male$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
vanderbilt_female$Comorbidity = c("hypertension_uncomplicated", "renal_failure")
data2 = as.data.frame(rbind(vanderbilt_male, vanderbilt_female))
data2$infant_sex = c(rep("Male", 2), rep("Female", 2))

data3 = as.data.frame(rbind(cedar_sinai_male, cedar_sinai_female))
data3$infant_sex = c(rep("Male",2), rep("Female", 2))

data = data[, 2:ncol(data)]
data = rbind(data, data2, data3)
data$institute = c(rep("UMich",4), rep("Vanderbilt",4), rep("Cedars-Sinai", 4))
data$institute = factor(data$institute, levels = c("Cedars-Sinai",  "Vanderbilt", "UMich"))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","renal_failure"))
data = data%>%filter(!(Comorbidity == "renal_failure" & institute == "Cedars-Sinai"))

figure5 = ggplot(data=data, aes(x=exp(LogitCoef), y = institute, xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = infant_sex, group = infant_sex)) +
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme_bw()+
  theme(strip.text.y= element_text(angle = 0),axis.title = element_text(size = 13), axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text = element_text(size = 13), strip.text = element_text(size = 13), legend.text=element_text(size=13))
figure5
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/PE_long_term_maternal_outcome/figures/combined_infant_sex0519.png", height = 4, width = 12, dpi = 600)
