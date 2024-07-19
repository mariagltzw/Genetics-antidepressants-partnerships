library("flexsurv")
library(survival)
library(dplyr)

#set working directory
#setwd()

#load df
load("test_data.RData")

# CHECK ERROR DISTRIBUTION  -----------------------------------
# fit AFT model with different error distributions
#weibull
aft2 <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                       SEX + age + educ_baseline,
                     dist="weibull", 
                     data=df_aft_ord)
#exponential
aft2a <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                                  pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                                  SEX + age + educ_baseline,
                     dist="exponential", 
                     data=df_aft_ord)

# log logistic
aft2b <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                             pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                             SEX + age + educ_baseline,
                     dist="llogis",
                     data=df_aft_ord)

#lognormal
aft2c <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                             pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                             SEX + age + educ_baseline,
                     dist="lognormal",
                     data=df_aft_ord)


#plot all models against observed in one plot to visually assess error distribution
par(mfrow=c(2,2))
plot(Surv(df_aft_ord$fu_time,df_aft_ord$fu_time2,df_aft_ord$antidepress_med_yr_rec), ylim=c(0.5,1),
     main="Weibull distribution")
lines(aft2, ylim=c(0.5,1))

plot(Surv(df_aft_ord$fu_time,df_aft_ord$fu_time2,df_aft_ord$antidepress_med_yr_rec), ylim=c(0.5,1),
     main="Exponential distribution")
lines(aft2a, col="green") 

plot(Surv(df_aft_ord$fu_time,df_aft_ord$fu_time2,df_aft_ord$antidepress_med_yr_rec), ylim=c(0.5,1),
     main="Loglogistic distribution")
lines(aft2b, col="blue")

plot(Surv(df_aft_ord$fu_time,df_aft_ord$fu_time2,df_aft_ord$antidepress_med_yr_rec), ylim=c(0.5,1),
     main="Lognormal distribution")
lines(aft2c, col="purple")

#visually all fit well, we chose exponential but lognormal would also be a good fit

# check AIC curves
AIC(aft2, aft2a, aft2b, aft2c) 

