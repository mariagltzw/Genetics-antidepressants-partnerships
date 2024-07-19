library(dplyr)
library(survival)
library(flexsurv)

# set working directory
#setwd()

#load dataset
load("test_data.RData")

#we only fit the "final" models for the subgroup analysis by gender (M3 and M4)
#M3 for female and male seperately
aft3.F <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                       pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                       age + educ_baseline,
                     dist="exponential",
                    data=df_aft_ord[df_aft_ord$SEX=="female",])
aft3.M <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                        pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                        age + educ_baseline,
                      dist="exponential",
                      data=df_aft_ord[df_aft_ord$SEX=="male",])

#M4 for female and male seperately
aft4.M <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                        pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                        age + educ_baseline + 
                        pgs_howard_cat:age + pgs_howard_cat:educ_baseline +
                        marital_stat_cohab:age + marital_stat_cohab:educ_baseline,
                      dist="exponential",
                      data=df_aft_ord[df_aft_ord$SEX=="male",])

aft4.F <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat * marital_stat_cohab +
                        pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                        age + educ_baseline + 
                        pgs_howard_cat:age + pgs_howard_cat:educ_baseline +
                        marital_stat_cohab:age + marital_stat_cohab:educ_baseline,
                      dist="exponential",
                      data=df_aft_ord[df_aft_ord$SEX=="female",])


#save models 
models1to4_gender <- list(M3.F = aft3.F, 
                   M3.M = aft3.M,
                   M4.F = aft4.F,
                   M4.M = aft4.M)
save(file="models1to4_subgroup.RData", models1to4_gender)


