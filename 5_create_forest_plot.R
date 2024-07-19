library(dplyr)
library(ggplot2)
library("patchwork")
library("ggtext")
library(cowplot)

#set working directory
#setwd()

#load main results
main <- read.csv("predicted.cum.hazard.csv", header = T)

#remove empty column
main[,1] <- NULL

### MAIN ANALYSIS ####
#sort df so we can reshape it and make the figures
main <- main[,c(1,2,
                3,5,7,9,
                4,6,8,10)]
#reshape to long
main <- reshape(main, direction="long",
                varying = names(main[3:10]),
                timevar = "model")
main$pred <- as.numeric(main$pred)
main$se <- as.numeric(main$se)

#calculate lower and upper 95% confidence intervals
main$lb <- main$pred - 1.96*main$se
main$ub <- main$pred + 1.96*main$se

#recode some factors
df <- main
df$marital_stat_cohab <- factor(df$marital_stat_cohab,
                                levels = c("cohabiting","married","single","divorced","widowed"))
df$marital.stat <- factor(df$marital_stat_cohab,
                          labels = c("Cohabiting","Married","Single","Divorced","Widowed"))
df$pgs_howard_cat <- factor(df$pgs_howard_cat, labels = c("<=20th","21st to 79th", ">=80th"))
df$PGS <- df$pgs_howard_cat

#set colorblind friendly color palette
cbpalette <- c("Orange","#999999","Skyblue")

#plot
plot <- df %>%
  ggplot(aes(y=marital.stat, x=pred, shape=PGS, color=PGS)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(aes(xmin=lb, xmax=ub), position = position_dodge2(width=0.3)) +
  theme_minimal() +
  panel_border()+
  theme(strip.text = element_textbox(
    size = 13,
    color = "black", fill = "white", box.color = "#C5C5C5",
    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
    axis.title.y = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        text= element_text(size=13)) +
  scale_color_manual(values = cbpalette) +
  scale_shape_manual(values = c(3,4,6))+
  guides(shape="none",
         color=guide_legend(title="PGI percentile"))+
  labs(x="\npredicted cumulative hazard at year 10", y="") +
  facet_wrap( ~ model, nrow=2,
              labeller = as_labeller(c("M1"="Model 1", "M2" = "Model 2",
                                       "M3" = "Model 3.2", "M4" = "Model 4")))

ggsave("Main_results_plot_year10.svg", plot, width=7, height=7)

### SUBGROUP ANALYSIS ####
subgroup <- read.csv("predicted.cum.hazard_SUBGROUP.csv", header = T)
#remove empty column
subgroup[,1] <- NULL

colnames(subgroup) <- sub("3.F", "1", colnames(subgroup))
colnames(subgroup) <- sub("4.F", "2", colnames(subgroup))
colnames(subgroup) <-  sub(".M$", "", colnames(subgroup))

#update df so we can make figures with it
subgroup <- subgroup[,c(1,2,
                3,7,5,9,
                4,8,6,10)]

#reshape to long
subgroup <- reshape(subgroup, direction="long",
                varying = names(subgroup[3:10]),
                timevar = "model")

subgroup$pred <- as.numeric(subgroup$pred)
subgroup$se <- as.numeric(subgroup$se)
#create 95% Confidence intervals
subgroup$lb <- subgroup$pred - 1.96*subgroup$se
subgroup$ub <- subgroup$pred + 1.96*subgroup$se

#re-assign sex
subgroup$SEX <- c(rep("female",30),rep("male",30))
subgroup$model <- ifelse(subgroup$model=="M1","M3",
                         ifelse(subgroup$model=="M2","M4", subgroup$model))

df <- subgroup
df$marital_stat_cohab <- factor(df$marital_stat_cohab,
                                levels = c("cohabiting","married","single","divorced","widowed"))
df$marital.stat <- factor(df$marital_stat_cohab,
                          labels = c("Cohabiting","Married","Single","Divorced","Widowed"))
df$pgs_howard_cat <- factor(df$pgs_howard_cat, labels = c("<=20th","21st to 79th", ">=80th"))
df$PGS <- df$pgs_howard_cat

#set color blind friendly color palette
cbpalette <- c("Orange","#999999","Skyblue")

plot <- df %>%
  ggplot(aes(y=marital.stat, x=pred, shape=PGS, color=PGS)) +
  geom_point(position = position_dodge2(width = 0.3)) +
  geom_linerange(aes(xmin=lb, xmax=ub), position = position_dodge2(width=0.3)) +
  theme_minimal() +
  panel_border()+
  theme(strip.text = element_textbox(
    size = 13,
    color = "black", fill = "white", box.color = "#C5C5C5",
    halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
    padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)),
    axis.title.y = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        text= element_text(size=13),
        strip.text.y = element_text(angle = 270)) +
  scale_color_manual(values = cbpalette) +
  scale_shape_manual(values = c(3,4,6))+
  guides(shape="none",
         color=guide_legend(title="PGI percentile"))+
  labs(x="\npredicted cumulative hazard at year 10", y="") +
  facet_grid(SEX ~ model,
              labeller = as_labeller(c("female" ="Female", "male"="Male",
                                       "M3" = "Model 3.2", "M4" = "Model 4")))

ggsave("subgroup_results_plot_year10.svg", plot, width=7, height=7)

