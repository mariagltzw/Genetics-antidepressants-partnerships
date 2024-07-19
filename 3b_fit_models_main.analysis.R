library(dplyr)
library(ggplot2)
library(survival)
library(flexsurv)
library(ggpubr)

#set working directory
#setwd()

#load df
load("test_data.RData")

## fit model 1####
aft1 <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat + marital_stat_cohab +
                      pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch+  
                      age + SEX,
                    dist="exponential",data=df_aft_ord)
aft1
## fit model 2 ####
aft2 <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                      pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch+  
                      age + SEX,
                    dist="exponential",
                    data=df_aft_ord)

## fit model 3####
aft3 <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                       SEX + age + educ_baseline,
                     dist="exponential",
                    data=df_aft_ord)


## fit model 4####
aft4 <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                          pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                          SEX + age + educ_baseline + 
                          pgs_howard_cat:SEX + pgs_howard_cat:age + pgs_howard_cat:educ_baseline +
                          marital_stat_cohab:SEX + marital_stat_cohab:age + marital_stat_cohab:educ_baseline,
                        dist="exponential",
                    data=df_aft_ord)

#save all these models 
models1to4 <- list(M1 = aft1, 
                   M2_int = aft2,
                   M3_educ = aft3,
                   Keller.adj = aft4)

save(file="models1to4.RData", models1to4)
rm(aft1);rm(aft2);rm(aft3);rm(aft4)
gc()

