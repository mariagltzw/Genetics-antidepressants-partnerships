library(dplyr)
library(flexsurv)

# set working directory
#setwd()

#load dataset
load("test_data.RData")

# load models
load("models1to4.RData")

#create dataset with every combination of PGS, marital status, sex, education, data_batch, and multiple ages
#we set the principal components as their mean for prediction
newdat <- expand.grid(pgs_howard_cat = levels(df_aft_ord$pgs_howard_cat),  
                             marital_stat_cohab = levels(df_aft_ord$marital_stat_cohab),
                             pc1=mean(df_aft_ord$pc1),pc2=mean(df_aft_ord$pc2),
                             pc3=mean(df_aft_ord$pc3),pc4=mean(df_aft_ord$pc4),
                             pc5=mean(df_aft_ord$pc5),pc6=mean(df_aft_ord$pc6),
                             pc7=mean(df_aft_ord$pc7),pc8=mean(df_aft_ord$pc8),
                             pc9=mean(df_aft_ord$pc9),pc10=mean(df_aft_ord$pc10),
                             SEX=levels(df_aft_ord$SEX), educ_baseline=levels(df_aft_ord$educ_baseline),
                             age=c(25,35,45,55,65,75,80), data_batch=levels(df_aft_ord$data_batch))

#### make a results table that shows cum. hazard at t 10, ... ####
#type="survival" for survival probabilities
#type="cumhaz" for cumulative hazards

#M1
#predict for time point t=10
pred_M1 <- predict(models1to4$M1, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)
plot_df.M1 <- cbind(newdat, pred_M1)

#aggregate across partnership status and PGI
plot_df.M1.avg <- plot_df.M1 %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M1.avg$model <- "M1"
rm(plot_df.M1)

#M2
#predict for time point t=10
pred_M2 <- predict(models1to4$M2_int, newdata = newdat,
                   times = 10,
                   type="cumhaz", se.fit = T)
plot_df.M2 <- cbind(newdat, pred_M2)

#aggregate across partnership status and PGI
plot_df.M2.avg <- plot_df.M2 %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M2.avg$model <- "M2"

rm(plot_df.M2)

#M3
#predict for time point t=10
pred_M3 <- predict(models1to4$M3_educ, newdata = newdat,
                     times = 10,#c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                     type="cumhaz", se.fit = T)

plot_df.M3 <- cbind(newdat, pred_M3)

#aggregate across partnership status and PGI
plot_df.M3.avg <- plot_df.M3 %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M3.avg$model <- "M3"
rm(plot_df.M3)

#M4
#predict for time point t=10
pred_M4 <- predict(models1to4$Keller.adj, newdata = newdat,
                     times = 10,
                     type="cumhaz", se.fit = T)

plot_df.M4 <- cbind(newdat, pred_M4)

#aggregate across partnership status and PGI
plot_df.M4.avg <- plot_df.M4 %>% group_by(marital_stat_cohab, pgs_howard_cat) %>%
  summarise(pred = mean(.pred_cumhaz),
            se = mean(.std_error))
plot_df.M4.avg$model <- "M4"
rm(plot_df.M4)

# merge all predictions into one data frame
prelim.1 <- merge(plot_df.M1.avg, plot_df.M2.avg, by=c("pgs_howard_cat","marital_stat_cohab"))
prelim.1 <- prelim.1 %>% rename(pred.M1 = pred.x, se.M1=se.x, 
                                pred.M2 = pred.y, se.M2=se.y)
prelim.2 <- merge(prelim.1, plot_df.M3.avg, by=c("pgs_howard_cat","marital_stat_cohab"))
table.df <- merge(prelim.2, plot_df.M4.avg, by=c("pgs_howard_cat","marital_stat_cohab"))

#remove the columns that show which model was fitted
table.df$model.x <- NULL 
table.df$model.x <- NULL 
table.df$model.y <- NULL 
table.df$model.y <- NULL 

#rename some columns so it's easier to read
table.df <- table.df %>% mutate_if(is.numeric,round,4)

table.df <- table.df %>% 
  rename(pred.M3 = pred.x, se.M3=se.x, 
         pred.M4 = pred.y, se.M4=se.y) 

write.csv(file="predicted.cum.hazard.csv",table.df)

