library(dplyr)
library(survival)
library(flexsurv)

# set working directory
#setwd()

#load models 1 to 4 since we already fit those earlier
load(file="models1to4.RData")

#load dataset
load("test_data.RData")

# log likelhood test - all compared to model 1####
ll.M1 <- as.numeric(logLik(models1to4$M1))
df.M1 <-models1to4$M1$npars 
ll.M2 <- as.numeric(logLik(models1to4$M2_int))
df.M2 <- models1to4$M2_int$npars 
df.diff <- df.M2-df.M1
ll2 <- -2*(ll.M1-ll.M2)
p2 <- 1-pchisq(ll2,(df.M2-df.M1)) #adding interaction term is not sign improving model fit

# fit model 3 without interaction
aft3_noint <- flexsurvreg(Surv(fu_time,fu_time2,antidepress_med_yr_rec) ~ pgs_howard_cat + marital_stat_cohab +
                      pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + data_batch +  
                      SEX + age + educ_baseline,
                    dist="exponential",
                    data=df_aft_ord)

ll.M3 <- as.numeric(logLik(aft3_noint))
df.M3 <- aft3_noint$npars 
ll.M3a <- as.numeric(logLik(models1to4$M3_educ))
df.M3a <- models1to4$M3_educ$npars
df.diff2 <- df.M3a-df.M3
ll3 <- -2*(ll.M3-ll.M3a)
p3 <- 1-pchisq(ll3,(df.M3a-df.M3)) #adding interaction term is not sign improving model fit

#create output table 
output.table <- data.frame(model = c(rep("M1 compared to M2",2),rep("M3 without int compared to M3 with int",2)),
                           df = c(df.M1, df.M2, df.M3a, df.M3), 
                           loglik = c(ll.M1, ll.M2, ll.M3a, ll.M3),
                           df.diff = c(rep(df.diff,2), rep(df.diff2,2)),
                           teststat = c(rep(ll2,2), rep(ll3,2)),
                           p_value = c(rep(p2,2), rep(p3,2)))


# save
write.csv(output.table, file="test.model.fit.exp_RR.csv")
