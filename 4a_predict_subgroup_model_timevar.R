library(dplyr)
library(survival)
library(flexsurv)

# set working directory
#setwd()

#load models
load("models1to4_subgroup.RData")

#load dataset
load("test_data.RData")

#create dataset with every combination of PGS, marital status, education, data_batch, and multiple ages
#we set the principal components as their mean for prediction
newdat <- expand.grid(pgs_howard_cat = levels(df_aft_ord$pgs_howard_cat), 
                      marital_stat_cohab = levels(df_aft_ord$marital_stat_cohab),
                      pc1=mean(df_aft_ord$pc1),pc2=mean(df_aft_ord$pc2),
                      pc3=mean(df_aft_ord$pc3),pc4=mean(df_aft_ord$pc4),
                      pc5=mean(df_aft_ord$pc5),pc6=mean(df_aft_ord$pc6),
                      pc7=mean(df_aft_ord$pc7),pc8=mean(df_aft_ord$pc8),
                      pc9=mean(df_aft_ord$pc9),pc10=mean(df_aft_ord$pc10),
                      educ_baseline=levels(df_aft_ord$educ_baseline),
                      age=c(25,35,45,55,65,75,80), data_batch=levels(df_aft_ord$data_batch))

#M3
#predict for time point t=10 for females and males seperately
#females
pred_M3.F <- predict(models1to4_gender$M3.F, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)

plot_df.M3.F <- cbind(newdat, pred_M3.F)

#aggregate across partnership status and PGI
plot_df.M3.F.avg <- plot_df.M3.F %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M3.F.avg$model <- "M3.F"
rm(plot_df.M3.F)

#males
pred_M3.M <- predict(models1to4_gender$M3.M, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)
plot_df.M3.M <- cbind(newdat, pred_M3.M)

#aggregate across partnership status and PGI
plot_df.M3.M.avg <- plot_df.M3.M %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M3.M.avg$model <- "M3.M"

rm(plot_df.M3.M)

#M4
#predict for time point t=10 for females and males seperately
#females
pred_M4.F <- predict(models1to4_gender$M4.F, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)

plot_df.M4.F <- cbind(newdat, pred_M4.F)

#aggregate across partnership status and PGI
plot_df.M4.F.avg <- plot_df.M4.F %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M4.F.avg$model <- "M4.F"
rm(plot_df.M4.F)

#males
pred_M4.M <- predict(models1to4_gender$M4.M, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)

plot_df.M4.M <- cbind(newdat, pred_M4.M)

#aggregate across partnership status and PGI
plot_df.M4.M.avg <- plot_df.M4.M %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M4.M.avg$model <- "M4.M"
rm(plot_df.M4.M)

# merge all predictions into one data frame
prelim.1 <- merge(plot_df.M3.F.avg, plot_df.M3.M.avg, by=c("pgs_howard_cat","marital_stat_cohab"))
prelim.1 <- prelim.1 %>% rename(pred.M3.F = pred.x, se.M3.F=se.x, 
                                pred.M3.M = pred.y, se.M3.M=se.y)
prelim.2 <- merge(prelim.1, plot_df.M4.F.avg, by=c("pgs_howard_cat","marital_stat_cohab"))
table.df <- merge(prelim.2, plot_df.M4.M.avg, by=c("pgs_howard_cat","marital_stat_cohab"))

#remove the columns that show which model was fitted
table.df$model.x <- NULL 
table.df$model.x <- NULL 
table.df$model.y <- NULL 
table.df$model.y <- NULL 

#rename some columns so it's easier to read
table.df <- table.df %>% mutate_if(is.numeric,round,4)

table.df <- table.df %>% 
  rename(pred.M4.F = pred.x, se.M4.F=se.x,
         pred.M4.M = pred.y, se.M4.M=se.y) 

#save
write.csv(file="predicted.cum.hazard_SUBGROUP.csv",table.df)

