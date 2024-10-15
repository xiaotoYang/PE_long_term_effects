library(tidyverse)
library(reshape2)
library(gridExtra)
library(corrplot)
library(qacReg)
load("/nfs/turbo/umms-lgarmire2/Xiaotong/10yr_odds_ratio/sign_results_elix_list.RData")

ggplot(data=sign_results_elix_list[[5]], aes(y= reorder(Comorbidity, LogitRegP), x=odds, xmin=omin, xmax=omax)) +
  theme_minimal()+
  geom_errorbarh(height=.1)+
  geom_point(color = "red") + 
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Odds Ratios of Case and Control, Elixhauser Comorbidities", x = "Odds Ratio", y = "Comorbidity")


for (i in 1:10) {
  result_elix_list[[i]] = result_elix_list[[i]] %>% arrange(LogitRegP)
  temp = ggplot(data=result_elix_list[[i]], aes(y= reorder(Comorbidity, LogitRegP), x=odds, xmin=omin, xmax=omax)) +
    theme_minimal()+
    geom_errorbarh(height=.2)+
    geom_point(color = "#BC5340") + 
    geom_vline(xintercept = 1, linetype = "dashed") +
    labs(title = paste0("Odds Ratios of Case and Control, ", i, " Years"), x = "Odds Ratio", y = "Comorbidity") +
    theme(axis.text.y = element_text(face = ifelse(result_elix_list[[i]]$PropTestP < 0.05, "bold", "plain"))) +
    theme(axis.text.y = element_text(color = ifelse(result_elix_list[[i]]$LogitRegP < 0.05, "#BC5340", "black")))
  ggsave(paste0("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/OddsRatio_", i, ".png"), height = 8, width = 12)
}


### OR

trend_rate_case = data.frame(Comorbidity = result_elix_list[[1]]$Comorbidity)
trend_rate_control = data.frame(Comorbidity = result_elix_list[[1]]$Comorbidity)

for (i in 1:10) {
  temp = result_elix_list[[i]] %>% select(Comorbidity, Rate.x)
  trend_rate_case = trend_rate_case %>%
    full_join(temp, by = "Comorbidity")
  colnames(trend_rate_case)[i+1] = paste0(i)
  
  temp = result_elix_list[[i]] %>% select(Comorbidity, Rate.y)
  trend_rate_control = trend_rate_control %>%
    full_join(temp, by = "Comorbidity")
  colnames(trend_rate_control)[i+1] = paste0(i)
}

trend_rate_case_long <- melt(trend_rate_case, id.vars = "Comorbidity", variable.name = "Year", value.name = "Rate") %>%
  mutate(Group = "case")
trend_rate_case_long$Year = as.integer(trend_rate_case_long$Year)

trend_rate_control_long <- melt(trend_rate_control, id.vars = "Comorbidity", variable.name = "Year", value.name = "Rate") %>%
  mutate(Group = "control")
trend_rate_control_long$Year = as.integer(trend_rate_control_long$Year)

trend_rate_long = bind_rows(trend_rate_case_long, trend_rate_control_long)

temp = ggplot(data = trend_rate_long, aes(x = Year, y = Rate, color = Group)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Comorbidity, scales = "fixed", nrow = 7) +
  xlab("Year") +
  ylab("Occurence Rate") +
  ggtitle("Trends of Elixhauser Comorbidities Occurence Rates During 1~10 Years") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/TrendRate.png", temp, height = 15, width = 15)

### Odds

trend_odds = data.frame(Comorbidity = result_elix_list[[1]]$Comorbidity)
trend_omin = data.frame(Comorbidity = result_elix_list[[1]]$Comorbidity)
trend_omax = data.frame(Comorbidity = result_elix_list[[1]]$Comorbidity)

for (i in 1:10) {
  temp = result_elix_list[[i]] %>% select(Comorbidity, odds)
  trend_odds = trend_odds %>%
    full_join(temp, by = "Comorbidity")
  colnames(trend_odds)[i+1] = paste0(i)
  
  temp = result_elix_list[[i]] %>% select(Comorbidity, omin)
  trend_omin = trend_omin %>%
    full_join(temp, by = "Comorbidity")
  colnames(trend_omin)[i+1] = paste0(i)
  
  temp = result_elix_list[[i]] %>% select(Comorbidity, omax)
  trend_omax = trend_omax %>%
    full_join(temp, by = "Comorbidity")
  colnames(trend_omax)[i+1] = paste0(i)
}

trend_odds_long <- melt(trend_odds, id.vars = "Comorbidity", variable.name = "Year", value.name = "Odds")
trend_odds_long$Year = as.integer(trend_odds_long$Year)

trend_omin_long <- melt(trend_omin, id.vars = "Comorbidity", variable.name = "Year", value.name = "omin")
trend_omin_long$Year = as.integer(trend_omin_long$Year)

trend_omax_long <- melt(trend_omax, id.vars = "Comorbidity", variable.name = "Year", value.name = "omax")
trend_omax_long$Year = as.integer(trend_omax_long$Year)

trend_odds = trend_odds_long %>% 
  left_join(trend_omin_long, by = c("Comorbidity", "Year")) %>%
  left_join(trend_omax_long, by = c("Comorbidity", "Year"))

trend_odds = trend_odds%>%filter(Comorbidity!= "Peptic Ulcer Disease Excluding Bleeding")

temp = ggplot(data = trend_odds, aes(x = Year, y = Odds, color = Comorbidity)) +
  ylim(0, max(trend_odds$omax)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_point() +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = omin, ymax = omax), width = 0.1) +
  geom_ribbon(aes(ymin = omin, ymax = omax, fill = Comorbidity), alpha = 0.2, color = NA) +
  facet_wrap(~ Comorbidity, scales = "free_y", nrow = 5) +
  xlab("Year") +
  ylab("Odds Ratio") +
  #ggtitle("Trends of Elixhauser Comorbidities Odds Ratio During 1~10 Years") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave("/nfs/turbo/umms-lgarmire2/Xiaotong/figures/TrendOdds.png", temp, height = 12, width = 18)


### Ages

# ages = all_final_elix_list[[1]] %>%
#   mutate(Group = if_else(PE == 1, "Case", "Control")) %>%
#   group_by(DeID_PatientID) %>%
#   reframe(AgeInYears = min(AgeInYears), Group = Group[1])
# t.test(ages$AgeInYears[ages$Group == "Case"], ages$AgeInYears[ages$Group == "Control"])
# 
# plot_ages = ggplot(ages, aes(x = Group, y = AgeInYears, fill = Group)) +
#   geom_boxplot() +
#   labs(title = "Age Distribution, 5 Years", x = "Group", y = "AgeInYears") +
#   theme_minimal()
# ggsave("./figures/Ages.png", plot_ages)
# 
# ### Comorbidities
# 
# comorbidities = all_final_elix_list[[1]] %>%
#   mutate(Group = if_else(PE == 1, "Case", "Control")) %>%
#   select(Group, "GestationalDiabetes":"RheumatoidArthritisCollagenVascularDiseases") %>%
#   group_by(Group) %>%
#   reframe(across("GestationalDiabetes":"RheumatoidArthritisCollagenVascularDiseases", mean))
# 
# comorbidities = tidyr::pivot_longer(comorbidities, cols = -Group, names_to = "Confounder", values_to = "Rate")
# 
# plot_como = ggplot(comorbidities, aes(x = Confounder, y = Rate, fill = Group)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Comorbidities History in Case and Control, 5 Years", x = "Comorbidities", y = "Rate") +
#   coord_flip()
# ggsave("./figures/Comorbidities.png", plot_como, height = 10, width = 12)
# 
# comorbidities = all_final_elix_list[[1]] %>%
#   mutate(Group = if_else(PE == 1, "Case", "Control")) %>%
#   select(Group, "GestationalDiabetes":"RheumatoidArthritisCollagenVascularDiseases") %>%
#   group_by(Group) %>%
#   reframe(across("GestationalDiabetes":"RheumatoidArthritisCollagenVascularDiseases", mean))
# 
# comorbidities = as.data.frame(t(select(comorbidities, -Group))) %>%
#   tibble::rownames_to_column("Comorbidity") %>%
#   group_by(Comorbidity) %>%
#   mutate(pvalue = prop.test(c(V1*pop_case_list[[1]], V2*pop_control_list[[1]]), c(pop_case_list[[1]], pop_control_list[[1]]))$p.value)
# write.csv(comorbidities, file = "comorbidities_stats.csv")
# 
# 
# ### VIF and corrplot
# 
# for (i in 1:10) {
#   # png(height = 600, width = 600, file = paste0("./figures/CorrelationPlot_", i, ".png"))
#   # plot_i = corrplot(cor_list[[i]], type = "upper", title = paste0("Correlation Map, ", i, " Years"), mar=c(0,0,2,0))
#   # dev.off()
#   
#   plot_i = vif_plot(model_list[[i]]) +
#     ggtitle(paste0("Variance Inflation Plot, ", i, " Years"))
#   ggsave(paste0("./figures/VIFPlot_", i, ".png"), plot_i, height = 6, width = 6)
# }
# 
# ### Other
# uset = array(character())
# iset = sign_results_elix_list[[1]]$Comorbidity
# for (i in 1:10) {
#   print(paste0(i, " Years"))
#   print(sign_results_elix_list[[i]]$Comorbidity)
#   uset = union(uset, sign_results_elix_list[[i]]$Comorbidity)
#   iset = intersect(iset, sign_results_elix_list[[i]]$Comorbidity)
# }
# print(uset)
# print(iset)
# 
# # Population
# pop_case = unlist(pop_case_list)
# pop_control = unlist(pop_control_list)
# pop_all = data.frame(Year = 1:10, case = pop_case, control = pop_control)
# 
# temp = ggplot(data = pop_all, aes(x = Year)) +
#   geom_point(aes(y = case, color = "case")) +
#   geom_line(aes(y = case, color = "case")) +
#   geom_point(aes(y = control, color = "control")) +
#   geom_line(aes(y = control, color = "control")) +
#   labs(title = "Populations in Case and Control Group after 1~10 Years", x = "Year", y = "Population", color = "Group") +
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
#   
# ggsave("./figures/Population.png", temp, height = 6, width = 8)
# 
# 
# 
# case_como_stats = case_diag_all_elix_list[[1]] %>%
#   distinct(DeID_PatientID, .keep_all = TRUE) %>%
#   select(GestationalDiabetes:ncol(case_diag_all_elix_list[[1]])) %>%
#   colSums(2:ncol(case_diag_all_elix_list[[1]]))
# case_como_stats = data.frame(Comorbidity = names(case_como_stats), case = case_como_stats/pop_case_list[[1]])
# 
# control_como_stats = control_diag_all_elix_list[[1]] %>%
#   distinct(DeID_PatientID, .keep_all = TRUE) %>%
#   select(GestationalDiabetes:ncol(control_diag_all_elix_list[[1]])) %>%
#   colSums(2:ncol(control_diag_all_elix_list[[1]]))
# control_como_stats = data.frame(Comorbidity = names(control_como_stats), control = control_como_stats/pop_control_list[[1]])
# 
# como_stats = case_como_stats %>%
#   left_join(control_como_stats, by = "Comorbidity") %>%
#   group_by(Comorbidity) %>%
#   mutate(pvalue = prop.test(c(case*pop_case_list[[1]], control*pop_control_list[[1]]), c(pop_case_list[[1]], pop_control_list[[1]]))$p.value)
# 
# write.csv(como_stats, file = "como_stats.csv")
