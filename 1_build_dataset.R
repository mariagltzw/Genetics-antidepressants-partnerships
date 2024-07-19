########################################################
################### DATA HANDLING FOR ################## 
########### GENETIC PROPENSITY TO DEPRESSION ########### 
################### GUELTZOW ET AL #####################
########################################################

#### create sample according to flowchart in Figure S.1 of the paper
#load packages
library("haven")
library("readr")
library("dplyr")
library(Hmisc)

#set working directory
#setwd()

#load lead1 function 
lead1.func <- function(variabletolead, idvariable, timevariable){
  lead <- c(ifelse(idvariable[1:length(idvariable)-1] == idvariable[2:(length(idvariable))] &
                     timevariable[1:length(timevariable)-1] - timevariable[2:(length(timevariable))]== -1,
                   variabletolead[2:(length(idvariable))],NA),NA)
  return(lead)
}

#### Start with participants of the FINRISK/ HEALTH 2000 and 2011 surveys----
## FINRISK ####
FINRISK_92 <- read_dta("FINRISK_1992_data.dta")
FINRISK_92 <- FINRISK_92[,c(1,15,44:62)] #should have all English coded variables
FINRISK_92 <- FINRISK_92 %>% rename(sample_collection = sample_collectiony, year=vuosi,
                                    thl_id=bbid)
#set depression to NA for now so we can merge all FINRISK and HEALTH surveys 
FINRISK_92$depression <- NA
FINRISK_92 <- FINRISK_92[,c(1:13,22,14:21)]

FINRISK_97 <- read_dta("FINRISK_1997_data.dta")
FINRISK_97 <- FINRISK_97[,c(1,2,15,50:67)] # should have all English coded variables
FINRISK_97 <- FINRISK_97 %>% rename(thl_id=bbid, year=vuosi)
#set depression to NA for now so we can merge all FINRISK and HEALTH surveys 
FINRISK_97$depression <- NA
FINRISK_97 <- FINRISK_97[,c(1:13,22,14:21)]

FINRISK_02 <- read_dta("FINRISK_2002_data.dta")
FINRISK_02 <- FINRISK_02[,c(1,2,15,48:66)] # should have all English coded variables
FINRISK_02 <- FINRISK_02 %>% rename(thl_id=bbid, year=vuosi)

FINRISK_07 <- read_dta("FINRISK_2007_data.dta")
FINRISK_07 <- FINRISK_07[,c(1,2,15,48:64,32,65)] # should have all English coded variables
FINRISK_07 <- FINRISK_07 %>% rename(thl_id=bbid, year=vuosi)


FINRISK_12 <- read_dta("FINRISK_2012_data.dta")
FINRISK_12 <- FINRISK_12 %>% rename(thl_id=bbid, year=vuosi, sample_collection=sample_collectionx)
FINRISK_12 <- FINRISK_12[,colnames(FINRISK_07)] # should have all English coded variables

#merge all FINRISK surveys into one
FINRISK <- rbind(FINRISK_92, FINRISK_97, FINRISK_02, FINRISK_07, FINRISK_12)
rm(FINRISK_07);rm(FINRISK_02);rm(FINRISK_97);rm(FINRISK_92);rm(FINRISK_12)

#save column names to select the same for the Health surveys
colnamess <- colnames(FINRISK)

## HEALTH 2000 and 2011 SURVEYS ####
HEALTH_00 <- read_dta("Health_2000_data.dta")
HEALTH_00$year <- 2000
HEALTH_00$sample_collection <- "HEALTH 2000"
HEALTH_00 <- HEALTH_00 %>% rename(thl_id=bbid)

HEALTH_00 <- HEALTH_00[,colnamess]

HEALTH_11 <- read_dta("Health_2011_data.dta")
HEALTH_11$year <- 2011
HEALTH_11$sample_collection <- "HEALTH 2011"
HEALTH_11$hip <- NA 
HEALTH_11 <- HEALTH_11 %>% rename(thl_id=bbid)

HEALTH_11 <- HEALTH_11[,colnamess]

#join with FINRISK
FINRISK_HEALTH <- rbind(FINRISK, HEALTH_00, HEALTH_11)

rm(HEALTH_00);rm(HEALTH_11)

#rename the survey recorded ages before we add on the register data
FINRISK_HEALTH <- FINRISK_HEALTH %>% rename(age_at_survey=age,
                                  marital_status_at_survey = marital_status)

#### load polygenic risk scores for depression and remove IDs that don't have data ----
gen_dat_pgs <- read_dta("pgs_depression.dta")

FINRISK_HEALTH_gen <- inner_join(FINRISK_HEALTH, gen_dat_pgs, by="thl_id")
length(unique(FINRISK_HEALTH_gen$thl_id)) #N: 37355

rm(gen_dat_pgs);rm(FINRISK_HEALTH)

#### load reimbursement data and remove those who don't have it -----
med_data2 <- read_dta("reimbursement_data.dta")

#recode year
med_data2$year <- substr(med_data2$otpvm, start=1, stop=4)
med_data2$year <- as.factor(med_data2$year)

#only take purchasing of NO6A or NO6CA
med_data2$depress_med <- ifelse(grepl("N06A|N06CA", med_data2$atc),1,0)

#group by year and id to create antidepressant purchasing variable
med_data <- med_data2%>% group_by(year, thl_id) %>%
  summarise(antidepress_med = sum(depress_med))

# at least one purchasing of antidepressants calendar year = 1
med_data$antidepress_med_yr <- as.factor(ifelse(med_data$antidepress_med == 0, 0, 1))
med_data$year <- as.numeric(as.character(med_data$year))

rm(med_data2)

#### load register data and remove those who don't have it -----
reg_basic <- read_dta("register_data_basic_until2019.dta")

# rename and only select the ones we need
reg_basic <- reg_basic %>%
  rename(year = vuosi,
         BIRTH_year = syntyv,
         DEATH_year = kuolv,
         SEX = sukup,
         age= ika,
         ses = sose,
         occupation_status = amas1,
         occupation_code = ammattikoodi_k,
         marital_stat = sivs,
         HH_members = akoko_k,
         HH_type = asty,
         N_child_below_18 = a18lkm_k,
         family_type = pety,
         dispoable_income= kturaha_k,
         main_activity = ptoim1,
         educ_degree = ututku_aste, #highest education degree
         educ_type= klaji_k2) 

# join with reimbursement data
FR_med <- left_join(reg_basic, med_data[,c(1,2,4)], by=c("thl_id","year"))

# join with survey and genetic data
FINRISK_HEALTH_gen.med.reg <- inner_join(FINRISK_HEALTH_gen, FR_med, by=c("thl_id"))

length(unique(FINRISK_HEALTH_gen.med.reg$thl_id)) #N: 37352

rm(reg_basic); rm(med_data)
rm(FINRISK_HEALTH_gen); rm(FR_med)

#### merge in principal components (PC) data ####
pca_fr <- read_dta("pca_finrisk.dta")
pca_fr2 <- read_dta("pca_h2000h2011.dta")

pca_fr <- rbind(pca_fr, pca_fr2)

#rename ID
pca_fr <- pca_fr %>% rename(thl_id=bbid)

# join with the rest of the data
FINRISK_HEALTH_gen.med.reg <- left_join(FINRISK_HEALTH_gen.med.reg, pca_fr, by="thl_id")

#### merge in badge/chip data ####
batch_fr <- read_dta("finrisk_batch_chip.dta")
batch_fr2 <- read_dta("h20002011_batch.dta")
batch_fr2$chip <- NA
batch_fr2 <- batch_fr2[,c(1,3,2)]

batch_fr <- rbind(batch_fr, batch_fr2)

batch_fr <- batch_fr %>% rename(thl_id=bbid)
FINRISK_HEALTH_gen.med.reg <- left_join(FINRISK_HEALTH_gen.med.reg, batch_fr, by="thl_id")

# rename dataset
df_reg_FR <- FINRISK_HEALTH_gen.med.reg

## make sure the variable classes are correct and everything is properly coded ----
df_reg_FR$SEX <- as.factor(df_reg_FR$SEX)
df_reg_FR$SEX <-  factor(df_reg_FR$SEX, labels=c("male","female"))

df_reg_FR$marital_stat <- as.factor(df_reg_FR$marital_stat)
df_reg_FR$marital_stat <- factor(df_reg_FR$marital_stat, labels = c("unmarried",
                                                                    "married, registered partnership or seperated",
                                                                    "divorced or divorced from registered partnership",
                                                                    "widowed, or widowed from registered partnership"))  

df_reg_FR$family_type <- as.factor(df_reg_FR$family_type)
df_reg_FR$family_type <- factor(df_reg_FR$family_type, labels = c("NA","married couple without children", "married couple with children", 
                                                                  "mother with children", "father with children","cohabiting couple with common children",
                                                                  "cohabiting couple with non-common children","cohabiting couple without children"))

df_reg_FR$educ_degree <- as.factor(df_reg_FR$educ_degree)
df_reg_FR$educ_degree <- factor(df_reg_FR$educ_degree, labels = c("NA","Upper secondary level", "Post-secondary non-tertiary education",
                                                                  "Short-cycle tertiary education", "Bachelor's or equivalent level", "Master's or equivalent level",
                                                                  "Doctoral or equivalent level"))

df_reg_FR$educ_degree_5cat <- as.factor(ifelse(df_reg_FR$educ_degree=="Upper secondary level"| df_reg_FR$educ_degree=="Post-secondary non-tertiary education", "2.secondary",
                                               ifelse(df_reg_FR$educ_degree=="Short-cycle tertiary education", "3.lower tertiary", 
                                                      ifelse(df_reg_FR$educ_degree=="Bachelor's or equivalent level", "4.tertiary",
                                                             ifelse(df_reg_FR$educ_degree=="Master's or equivalent level"|df_reg_FR$educ_degree=="Doctoral or equivalent level", "5.higher tertiary",
                                                                    "1.basic level education")))))          

#select only the variables needed
df_aft <- df_reg_FR[,c("thl_id","year.x","year.y","BIRTH_year","age","age_at_survey", "sample_collection","SEX","marital_stat", "family_type", 
                       "antidepress_med_yr", "pgs_dep_wray", "pgs_dep_howard",
                       "DEATH_year","educ_degree_5cat", "ses", "occupation_status", "occupation_code",
                       "dispoable_income", 
                       "pc1","pc2","pc3","pc4","pc5","pc6","pc7","pc8","pc9","pc10", "batch", "chip")] 
rm(df_reg_FR);rm(FINRISK); rm(FINRISK_HEALTH_gen.med.reg); rm(pca_fr); rm(pca_fr2); rm(batch_fr); rm(batch_fr2)

#rename the two year variables
df_aft <- df_aft %>% rename(survey_year= year.x, year=year.y)

# We have quite a lot of interval censoring in the medication data,
# because we don't have information for a certain year if no prescription was picked up
# so if antidepressant purchasing is NA, we actually code it as zero
df_aft$antidepress_med_yr_rec <- as.factor(ifelse(is.na(df_aft$antidepress_med_yr),0,
                                               as.numeric(as.character(df_aft$antidepress_med_yr))))

# recode polygenic risk scores into three groups

#PGS according to Howard et al
tert <- quantile(df_aft$pgs_dep_howard, probs = c(1/5,4/5))
df_aft$pgs_howard_cat <- as.factor(ifelse(df_aft$pgs_dep_howard <= tert[1], "1.<=20th",
                                          ifelse(df_aft$pgs_dep_howard > tert[1] & df_aft$pgs_dep_howard < tert[2],"21st to 79th",
                                                 ifelse(df_aft$pgs_dep_howard >= tert[2],"3.>=80th",NA))))
#PGS according to Wray et al.
tert2 <- quantile(df_aft$pgs_dep_wray, probs = c(1/5,4/5))
df_aft$pgs_wray_cat <- as.factor(ifelse(df_aft$pgs_dep_wray <= tert2[1], "1.<=20th",
                                         ifelse(df_aft$pgs_dep_wray > tert2[1] & df_aft$pgs_dep_wray < tert2[2],"21st to 79th",
                                                ifelse(df_aft$pgs_dep_wray >= tert2[2],"3.>=80th",NA))))

#PGS according to Howard et al with different cut-off
tert3 <- quantile(df_aft$pgs_dep_howard, probs = c(1/10,9/10))
df_aft$pgs_howard_cat2 <- as.factor(ifelse(df_aft$pgs_dep_howard <= tert3[1], "1.<=10th",
                                          ifelse(df_aft$pgs_dep_howard > tert3[1] & df_aft$pgs_dep_howard < tert2[2],"2.11th to 89th",
                                                 ifelse(df_aft$pgs_dep_howard >= tert3[2],"3.>=90th",NA))))

## add cohabitation into the marital status variable to create partnership status variable
table(df_aft$marital_stat, df_aft$family_type)

#categories: single, cohabiting (unmarried), married, divorced, widowed
df_aft$marital_stat_cohab <- as.factor(ifelse(df_aft$marital_stat!="married, registered partnership or seperated" & df_aft$family_type == "cohabiting couple with common children"|
                                                df_aft$marital_stat!="married, registered partnership or seperated" & df_aft$family_type == "cohabiting couple with non-common children"|
                                                df_aft$marital_stat!="married, registered partnership or seperated" & df_aft$family_type == "cohabiting couple without children", "cohabiting", #cohabiting
                                                 ifelse(df_aft$marital_stat=="unmarried" &  df_aft$family_type != "cohabiting couple with common children"|
                                                          df_aft$marital_stat=="unmarried" & df_aft$family_type != "cohabiting couple with non-common children"|
                                                          df_aft$marital_stat=="unmarried" & df_aft$family_type != "cohabiting couple without children", "single (not cohabiting or married)",#single (not cohabiting or married)
                                                        as.character(df_aft$marital_stat))))

df_aft$marital_stat_cohab <- factor(df_aft$marital_stat_cohab, labels = c("cohabiting","divorced","married, registered partnership or seperated", 
                                                                          "single (not cohabiting or married)", "widowed"))  
df_aft$marital_stat_cohab <- factor(df_aft$marital_stat_cohab, levels = c("married, registered partnership or seperated", "divorced",
                                                                          "widowed","cohabiting","single (not cohabiting or married)"))  
## combine data and batch into one variable
df_aft$data_batch <- as.factor(ifelse(!is.na(df_aft$sample_collection2),
                                      paste(df_aft$batch, df_aft$sample_collection2, sep="_"), NA))


# sort the dataframe by id just to makes sure it's the correct format
df_aft_ord <- df_aft[order(df_aft$thl_id, df_aft$year),]
rm(df_aft)

#### APPLY INCLUSION/EXCLUSION CRITERIA ACCORDING TO METHODS SECTION ----
#remove duplicate rows
df_aft_ord <- df_aft_ord %>% distinct(thl_id, year,.keep_all = T)

FINRISK_HEALTH_gen.med.reg.age <- df_aft_ord

## age eligible?
FINRISK_HEALTH_gen.med.reg.age <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$age>=25 & FINRISK_HEALTH_gen.med.reg.age$age<=80,]
length(unique(FINRISK_HEALTH_gen.med.reg.age$thl_id))  #N: 33456 people

# the Health survey in 2011 includes a sample of Health 2000 participants
# find those IDs and only keep the row in which they first appeared with the assumption that that's when they collected their genetic information
Health.survey <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$sample_collection2=="HEALTH",]
Health.survey2 <- Health.survey %>% group_by(thl_id, survey_year) %>% summarise(n=n())
#find any duplicate rows
which(duplicated(Health.survey2$thl_id))
#none are duplicated so we can go ahead


# inclusion criteria is: participated in survey and did not purchase antidepressants for 2 years.
# inception of medication base is 97 - so for FINRISK 92 and 97, we use 97 and baseline 99
# for 2000, we use period 98 to 2000, for 2002, we use 00 to 01
# make an indicator of whether someone needs to be excluded because they purchased AD during the baseline period
# divide data set into survey years to handle the inclusion/exclusion

# 99 and before
df_99 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year <= 1997, ]
length(unique(df_99$thl_id))

#find IDs to exclude
purch.AD <- df_99[df_99$year==1995 & df_99$antidepress_med_yr_rec==1|
                           df_99$year==1996 & df_99$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

#save data set without those IDs
df_99 <- df_99[df_99$thl_id %in% purch.AD$thl_id == F,]
length(unique(df_99$thl_id))

# now exclude all records before baseline because we do not use that period in the models
df_99 <- df_99[df_99$year >=1997,]
length(unique(df_99$thl_id))

# 2000
df_00 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year == 2000, ]
length(unique(df_00$thl_id))

#find IDs to exclude
purch.AD <- df_00[df_00$year==1998 & df_00$antidepress_med_yr_rec==1|
                           df_00$year==1999 & df_00$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

df_00 <- df_00[df_00$thl_id %in% purch.AD$thl_id == F,]
length(unique(df_00$thl_id))

# now exclude all records before baseline because we do not use that period in the models
df_00 <- df_00[df_00$year >=2000,]
length(unique(df_00$thl_id))

# 2002
df_02 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year ==2002, ]
length(unique(df_02$thl_id))

#find ids to exclude
purch.AD <- df_02[df_02$year==2000 & df_02$antidepress_med_yr_rec==1|
                           df_02$year==2001 & df_02$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

#save data set without those IDs
df_02 <- df_02[df_02$thl_id %in% purch.AD$thl_id == F,]
length(unique(df_02$thl_id))

# now exclude all records before baseline because we do not use that period in the models
df_02 <- df_02[df_02$year >=2002,]
length(unique(df_02$thl_id))

# 2007
df_07 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year == 2007, ]
length(unique(df_07$thl_id))

#find ids to exclude
purch.AD <- df_07[df_07$year==2005 & df_07$antidepress_med_yr_rec==1|
                           df_07$year==2006 & df_07$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

#save data set without those IDs
df_07 <- df_07[df_07$thl_id %in% purch.AD$thl_id == F,]
length(unique(df_07$thl_id))

# now exclude all records before baseline
df_07 <- df_07[df_07$year >=2007,]
length(unique(df_07$thl_id))

# 2011
df_11 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year == 2011, ]
length(unique(df_11$thl_id))

#find ids to exclude
purch.AD <- df_11[df_11$year==2009 & df_11$antidepress_med_yr_rec==1|
                           df_11$year==2010 & df_11$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

#save data set without those IDs
df_11 <- df_11[df_11$thl_id %in% purch.AD$thl_id == F,]
length(unique(df_11$thl_id))

# now exclude all records before baseline
df_11 <- df_11[df_11$year >=2011,]
length(unique(df_11$thl_id))

# 2012
df_12 <- FINRISK_HEALTH_gen.med.reg.age[FINRISK_HEALTH_gen.med.reg.age$survey_year == 2012, ]
length(unique(df_12$thl_id))

#find ids to exclude
purch.AD <- df_12[df_12$year==2010 & df_12$antidepress_med_yr_rec==1|
                           df_12$year==2011 & df_12$antidepress_med_yr_rec==1,] 
length(unique(purch.AD$thl_id))

#save data set without those IDs
df_12 <- df_12[df_12$thl_id %nin% purch.AD$thl_id,]
length(unique(df_12$thl_id))

# now exclude all records before baseline
df_12 <- df_12[df_12$year >=2012,]
length(unique(df_12$thl_id))

#merge the datasets back together
df_aft_ord_notdepress <- rbind(df_99, df_00, df_02, df_07, df_11, df_12)
length(unique(df_aft_ord_notdepress$thl_id))


#### exclude one of two individuals if they are genetically related according to our threshold of 0.1875 ----
# load genetic relatedness data
p_hat_fr <- read_dta("fr_pi_hat.dta")
p_hat_fr2 <- read_dta("health20002011_pi_hat.dta")

p_hat_fr <- rbind(p_hat_fr, p_hat_fr2)

# only keep the IDs with a p hat larger than 0.1875
p_hat_fr_rel <- p_hat_fr[p_hat_fr$pi_hat>0.1875,]
p_hat_fr_n.rel <- p_hat_fr[p_hat_fr$pi_hat<=0.1875,]

length(unique(p_hat_fr_rel$fid1))

# of those that are related, select only one ID of the pair
p_hat_fr_rel2 <- p_hat_fr_rel[,c(1,3)]
p_hat_fr_rel2a <- p_hat_fr_rel2[!duplicated(t(apply(p_hat_fr_rel2,1,sort))),]

#find IDs
exclude_ids <- unique(p_hat_fr_rel2a$fid1) #3217
exclude_ids2 <- unique(p_hat_fr_rel2a$fid2) #3206

#any overlap?
length(which(unique(p_hat_fr_rel2a$fid1) %in% unique(p_hat_fr_rel2a$fid2)))
length(which(unique(p_hat_fr_rel2a$fid2) %in% unique(p_hat_fr_rel2a$fid1)))

# only keep id's that are not related
df_aft_ord_notrelated <- df_aft_ord_notdepress[df_aft_ord_notdepress$thl_id %in% exclude_ids==F,]
length(unique(df_aft_ord_notrelated$thl_id)) #N: 30,319

#### calculate time to event since start of prescription database ####
#rename data set
df_aft_ord <- df_aft_ord_notrelated

# take the start  follow-up year
start_year <- df_aft_ord %>% group_by(thl_id) %>% slice_head(n=1)

# attach to dataframe
df_aft_ord <- left_join(df_aft_ord, start_year[,c("thl_id","year")], by="thl_id")

# make a follow up time indicator
df_aft_ord <- df_aft_ord %>% group_by(thl_id) %>% mutate(fu_time = year.x - year.y,
                                                         fu_time2 = 1 + year.x - year.y) 
#this way baseline = futime 0 and the rest follows

# calculate time to antidepressant purchasing or censoring for every ID
aft_df1 <- df_aft_ord %>% group_by(thl_id) %>% 
  summarise(time_to_depress = ifelse(antidepress_med_yr_rec==1,fu_time[antidepress_med_yr_rec==1],NA),
            time_to_censor  = max(fu_time))

## make sure that we include only the first episode of AD purchasing
#only keep the first antidepressant purchasing event, the rest we remove
time_to_depress.ep.1 <- aft_df1 %>% filter(!is.na(time_to_depress)) %>% group_by(thl_id) %>%
  mutate(time_to_depress.ep.1 = min(time_to_depress))
time_to_depress.ep.1 <- time_to_depress.ep.1[!duplicated(time_to_depress.ep.1$thl_id),]

df_aft_ord <- left_join(df_aft_ord, time_to_depress.ep.1[,c("thl_id","time_to_depress.ep.1")], by="thl_id")
df_aft_ord <- cbind(df_aft_ord, aft_df1[,"time_to_censor"])

# recode 
df_aft_ord$time_to_depress <- ifelse(is.na(df_aft_ord$time_to_depress.ep.1),
                                     df_aft_ord$time_to_censor,
                                     df_aft_ord$time_to_depress.ep.1)

## remove rows after someone purchased antidepressants
df_aft_ord <- df_aft_ord %>%
  group_by(thl_id) %>%
  mutate(first_row_index = min(which(antidepress_med_yr_rec==1)))

#if theres no event first_row_index == INF, lets change it to NA so it*s easier to deal with
df_aft_ord$first_row_index <- ifelse(df_aft_ord$first_row_index=="Inf",NA, df_aft_ord$first_row_index)

#now select only rows that are below or equal to first_row_index or is.na(first_row_index)
df_aft_ord2 <- df_aft_ord %>%
  filter(row_number() <= first_row_index | is.na(first_row_index))

df_aft_ord2$first_row_index <- NULL


#remove labels (haven generated labels might otherwise mess with our analysis later)
library(labelled)
df_aft_ord2 <- remove_var_label(df_aft_ord2)
df_aft_ord2 <- as.data.frame(df_aft_ord2)

# put AD purchasing in correct format
df_aft_ord2$antidepress_med_yr_rec <- as.numeric(as.character(df_aft_ord2$antidepress_med_yr_rec))

# make time indicator 
df_aft_ord <- df_aft_ord2 %>% group_by(thl_id) %>% mutate(time = rep(1:n()))


#### interval censoring medication data --------------------------------------
#lead fu time
df_aft_ord$lead_fu.time <- lead1.func(df_aft_ord$fu_time, df_aft_ord$thl_id, df_aft_ord$time)


#now that we have lead fu time we can do x = t+1 - t
df_aft_ord$int.censor.indicator <- df_aft_ord$lead_fu.time - df_aft_ord$fu_time

int_censored <- df_aft_ord %>% filter(int.censor.indicator>1)

df_aft_ord2 <- df_aft_ord[df_aft_ord$thl_id %in% int_censored$thl_id==F,]
length(unique(df_aft_ord2$thl_id))


# final sample -----------------------------------------------

# make sure there are no duplicate rows
df_aft_ord <- df_aft_ord %>% distinct(thl_id, year.x,.keep_all = T)

# make sure survey year is coded as a factor
df_aft_ord$survey_year <- as.factor(df_aft_ord$survey_year)

# we are interested in education at baseline, not across follow up
# take education level recorded at follow up for each ID and create educ_baseline variable
baseline <- df_aft_ord %>% group_by(thl_id) %>% slice_head(n=1)
baseline <- baseline %>% rename(educ_baseline=educ_degree_5cat)

df_aft_ord <- left_join(df_aft_ord, baseline[,c("thl_id","educ_baseline")], by=c("thl_id"))


save(file="df_aft_ord.RData", df_aft_ord)
