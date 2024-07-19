library(dplyr)
library(ggplot2)
library(survival)
library(ggpubr)
library(cowplot)
library(ggtext)

#setwd()

load("test_data.RData")

#recode labels and levels so the order is correct in the plots
df_aft_ord$marital_stat_cohab <- factor(df_aft_ord$marital_stat_cohab,
                                         labels = c("Married","Divorced","Widowed","Cohabiting","Single"))
df_aft_ord$marital_stat_cohab <- factor(df_aft_ord$marital_stat_cohab,
                                         levels = c("Cohabiting","Married","Single", "Divorced","Widowed"))

#fit KM curves for the PGI stratified by partnership status
KM1 <- survfit(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat,
               data=df_aft_ord[df_aft_ord$marital_stat_cohab=="Cohabiting",])
KM2 <- survfit(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat,
               data=df_aft_ord[df_aft_ord$marital_stat_cohab=="Married",])
KM3 <- survfit(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat,
               data=df_aft_ord[df_aft_ord$marital_stat_cohab=="Single",])
KM4 <- survfit(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat,
               data=df_aft_ord[df_aft_ord$marital_stat_cohab=="Divorced",])
KM5 <- survfit(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat,
               data=df_aft_ord[df_aft_ord$marital_stat_cohab=="Widowed",])

# we want the smoothed version of those plots
#create df with all KM curves in one
d1 <- data.frame(time=c(KM1$time, KM2$time, KM3$time, KM4$time, KM5$time), 
                 surv=c(KM1$surv, KM2$surv, KM3$surv, KM4$surv, KM5$surv),
                 marital.stat = rep(levels(df_aft_ord$marital_stat_cohab),each=length(KM1$surv)),
                 PGS = as.factor(rep(rep(1:3, each=length(KM1$surv)/3),5)))
# calculate cumulative incidence from survival probability
d1$cum.in <- 1-d1$surv

#label everything right to make plotting easier
d1$PGS <- factor(d1$PGS, labels = c("<=20th","21st to 79th", ">=80th"))
d1$marital.stat <- factor(d1$marital.stat, levels = c("Cohabiting","Married","Single","Divorced","Widowed"))

# make sure this graph starts at 0 we include a 0,0 for every marital stat group and every PGS
# make data frame with all levels of  pgs and marital stat
df.add <- data.frame(time=rep(0,15), 
                     surv=rep(1,15),
                     marital.stat = rep(levels(df_aft_ord$marital_stat_cohab),each=3),
                     cum.in=rep(0,15),
                     PGS = as.factor(rep(c("<=20th","21st to 79th", ">=80th"))))
d1 <- rbind(d1, df.add)

# smooth 
d2 <- d1%>%
  group_by(marital.stat, PGS) %>%
  mutate(x=list(spline(time, cum.in, n=100, method="natural")[["x"]]),
         y=list(spline(time, cum.in, n=100, method="natural")[["y"]])) %>%
  tidyr::unnest(cols=c("x","y"))

# plot
plot <- ggplot() +
  geom_point(data=d1, aes(time,cum.in, color=PGS,shape=PGS), size=1) +
  geom_line(data=d2,
            aes(x,y, color=PGS)) +
  #coord_cartesian(ylim=c(0,0.75)) +
  theme_minimal() +
  panel_border()+
  theme(strip.text = element_textbox(
          size=12,
          color="black", fill = "white", box.color="#C5C5C5",
          halign=0.5, linetype=1, r= unit(5,"pt"),
          width=unit(1,"npc"), 
          padding=margin(2,0,1,0), margin=margin(3,3,3,3)
          ),
        legend.position = "bottom",
        text = element_text(size = 13))+
  guides(color=guide_legend(title="PGI percentile"),
         shape="none")+
  scale_color_manual(values = c("Orange","#999999","Skyblue"))+
  scale_shape_manual(values = c(3,4,6))+
  labs(x="\nFollow-up time in years",
       y="Cum. probability of\nantidepressant purchasing\n") +
  facet_wrap(~marital.stat, ncol = 5)


ggsave(file="cumulative.inc_smooth.svg",plot, height=5, width = 10)  


