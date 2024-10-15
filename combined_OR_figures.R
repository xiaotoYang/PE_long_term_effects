library(ggplot2)
library(dplyr)
setwd("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables")

load("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/ukbb_valid_result.RData")#ukbiobank result
result_list <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/result_list.csv")
UM_result <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result.csv")

#UM_result = UM_result
result_list$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
UM_result$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                          "diabetes_uncomplicated", "renal_failure","obesity", "hypothyroidism")
valid_result$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")

result_list = result_list[,2:5]
UM_result = UM_result[,2:5]
names(valid_result) = names(UM_result)
data = as.data.frame(rbind(result_list,UM_result, valid_result))
data$institute = c(rep("Cedar Sinai", 6), rep("UMich", 6), rep("UKBiobank", 6))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","diabetes_complicated","renal_failure", "diabetes_uncomplicated","obesity", "hypothyroidism"))
#data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "renal_failure"))
data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_complicated"))
#data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "obesity"))

figure = ggplot(data=data, aes(y=institute, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), 
                               xmax=exp(LogitCoef+1.96*CoefStd), color = institute)) +
  #theme_minimal()+
  guides(color="none")+
  geom_errorbarh(height=0.3, position = position_dodge(width = 0.7), linewidth = 1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  #labs(x = "Odds Ratio", y = "Comorbidity") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme(strip.text.y= element_text(angle = 0))
figure

ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/combine_odds_1007.png", height = 4, width = 8, dpi = 600)

#race------------------------------------------------------------------
result_list_white <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/result_list_white.csv")
UM_result_white <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_caucasian.csv")
result_list_black <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/result_list_black.csv")
UM_result_black <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_AA.csv")
#load("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/ukbb_valid_result_race.RData")
#valid_result_race$Race = c(rep("Caucasian", 6), rep("African American", 6))
UM_result_white = UM_result_white[,2:5]
UM_result_white$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                                "diabetes_uncomplicated", "renal_failure","obesity", "hypothyroidism")
result_list_white$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
UM_result_black = UM_result_black[,2:5]
UM_result_black$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                                "diabetes_uncomplicated", "renal_failure","obesity", "hypothyroidism")
result_list_black$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")

data = as.data.frame(rbind(result_list_white,UM_result_white, result_list_black, UM_result_black))
data$race = c(rep("Caucasian", 12), rep("African American", 12))
#names(valid_result_race) = names(data)
#valid_result_race$Comorbidity =rep(c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated"),2)

#data = as.data.frame(rbind(data, valid_result_race))
data$institute = c(rep("Cedar Sinai", 6), rep("UMich", 6), rep("Cedar Sinai", 6), rep("UMich", 6))
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","diabetes_complicated","renal_failure", "diabetes_uncomplicated","obesity", "hypothyroidism"))
data = data%>%filter(!(race=="African American"&institute=="Cedar Sinai"&Comorbidity=="renal_failure"))
#data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "renal_failure"))
#data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_complicated"))
#data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "obesity"))

figure2 = ggplot(data=data, aes(y=institute, x=exp(LogitCoef), xmin=exp(LogitCoef-1.96*CoefStd), 
                               xmax=exp(LogitCoef+1.96*CoefStd), color = race, group = race)) +
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme(strip.text.y= element_text(angle = 0))
figure2
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/combine_odds_race1007.png", height = 4, width = 8, dpi = 600)


### PE by severity---------------------------------
result_list_mild <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/result_list_mild.csv")
result_list_severe <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/result_list_severe.csv")
UM_result_mild <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_mild.csv")
UM_result_severe <- read.csv("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/UM_result_severe.csv")

result_list_mild$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
result_list_severe$Comorbidity = c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated")
UM_result_mild = UM_result_mild[,2:5]
UM_result_mild$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                               "diabetes_uncomplicated", "renal_failure","obesity", "hypothyroidism")

UM_result_severe = UM_result_severe[,2:5]
UM_result_severe$Comorbidity = c("hypertension_uncomplicated", "diabetes_complicated", 
                                 "diabetes_uncomplicated", "renal_failure","obesity", "hypothyroidism")

data = as.data.frame(rbind(UM_result_mild,result_list_mild, UM_result_severe, result_list_severe))
data$severity = c(rep("mild", 12), rep("severe", 12))
#names(valid_result_severity) = names(data)
#data = as.data.frame(rbind(data, valid_result_severity))
#data$institute = c(rep("Cedar Sinai", 6), rep("UMich", 6), rep("Cedar Sinai", 6), rep("UMich", 6), rep("UKBiobank", 12))
data$institute = c( rep("UMich", 6), rep("Cedar Sinai", 6), rep("UMich", 6),rep("Cedar Sinai", 6))

data = data%>%filter(!(severity=="mild"&institute=="Cedar Sinai"&Comorbidity=="renal_failure"))
# data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "renal_failure"&severity =="severe"))
# data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_complicated"&severity =="severe"))
# #data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "obesity"))
# data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_uncomplicated"&severity =="severe"))
# data
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","diabetes_complicated","renal_failure", "diabetes_uncomplicated","obesity", "hypothyroidism"))
figure3 = ggplot(data=data, aes(x=exp(LogitCoef), y = institute, xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = severity, group = severity)) +
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme(strip.text.y= element_text(angle = 0))

figure3
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/combine_odds_severity1009.png", height = 4, width = 8, dpi = 600)

#cedar sinai and ukbiobank plot supplementary
library(dplyr)
library(ggplot2)

load("/nfs/turbo/umms-lgarmire2/Xiaotong/logit_tables/ukbb_valid_result_severity.RData")


valid_result_severity$Comorbidity =rep(c("hypertension_uncomplicated", "hypothyroidism", "renal_failure","obesity", "diabetes_complicated", "diabetes_uncomplicated"),2)
data = as.data.frame(rbind(result_list_mild, result_list_severe))
data$severity = c(rep("mild", 6), rep("severe", 6))
names(valid_result_severity) = names(data)
data = as.data.frame(rbind(data, valid_result_severity))
data$institute = c(rep("Cedar Sinai", 6), rep("Cedar Sinai", 6),  rep("UKBiobank", 12))
result_list_mild=result_list_mild%>%filter(Comorbidity!="congestive_heartfailure"&Comorbidity!="renal_failure")
data$Comorbidity = factor(data$Comorbidity, 
                          levels = c("hypertension_uncomplicated","diabetes_complicated","renal_failure", "diabetes_uncomplicated","obesity", "hypothyroidism"))
data = data%>%filter(!(severity=="mild"&institute=="Cedar Sinai"&Comorbidity=="renal_failure"))
data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "renal_failure"&severity =="severe"))
data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_complicated"&severity =="severe"))
data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "obesity"))
data = data%>%filter(!(institute == "UKBiobank"& Comorbidity == "diabetes_uncomplicated"&severity =="severe"))

figure4 = ggplot(data=data, aes(x=exp(LogitCoef), y = institute, xmin=exp(LogitCoef-1.96*CoefStd), 
                                xmax=exp(LogitCoef+1.96*CoefStd), color = severity, group = severity)) +
  geom_errorbarh(height=0.2, position = position_dodge(width = 0.5), linewidth=1)+
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_continuous(breaks = c(1,2,4,6))+
  facet_grid(Comorbidity~.)+
  theme(strip.text.y= element_text(angle = 0))

figure4
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/supple_severity1007.png", height = 4, width = 8, dpi = 600)
