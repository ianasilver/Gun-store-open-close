#GettingStarted ####
# Written Ay: Ian A. Silver
# Purpose: Gun violence and Dental

library(psych)
library(lavaan)
library("moments")
library("pastecs")
library("ggplot2")
library("extrafont")
library("data.table")
library(dplyr)
library(tidyverse)
library("sjmisc")
library(expss)
library(readstata13)
library(ggplot2)
library(dplyr)
library(lme4)
library(ggridges)
library(extrafont)
library(Hmisc)
library(psych)
library(lmerTest)
library(pbkrtest)
library(MuMIn)
library(pastecs)
library(pracma)
library(matrixStats)
library(purrr)
library(mice)
library(VIM)
library(tidyverse)
library(egg)
library(data.table)
library(doParallel)
library(parallel)
library(foreach)
library(doRNG)
library(mice)
library("gamlss")
library("gamlss.cens")
library(effsize)
detectCores()
registerDoParallel(2)

options(scipen=100, digits=12)
options(max.print = 500000000)



setwd("G:/My Drive/External Work/004-Dan & Richard Projects/003_Gun Store open closing violence")


# Loading CSV (Use Me)####
A<-fread("G:/My Drive/External Work/004-Dan & Richard Projects/Data_9.16.24/NSF DATA TRACTS 2020 wInspection.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")

# Cleanind Data ####

## Total Gun Violence Measure ####
A$fatal2014[is.na(A$fatal2014)]<-0
A$nonfatal2014[is.na(A$nonfatal2014)]<-0
stat.desc(A$fatal2014)
stat.desc(A$nonfatal2014)
A$total_2014<-A$fatal2014+A$nonfatal2014
stat.desc(A$total_2014)

A$fatal2015[is.na(A$fatal2015)]<-0
A$nonfatal2015[is.na(A$nonfatal2015)]<-0
stat.desc(A$fatal2015)
stat.desc(A$nonfatal2015)
A$total_2015<-A$fatal2015+A$nonfatal2015
stat.desc(A$total_2015)

A$fatal2016[is.na(A$fatal2016)]<-0
A$nonfatal2016[is.na(A$nonfatal2016)]<-0
stat.desc(A$fatal2016)
stat.desc(A$nonfatal2016)
A$total_2016<-A$fatal2016+A$nonfatal2016
stat.desc(A$total_2016)

A$fatal2017[is.na(A$fatal2017)]<-0
A$nonfatal2017[is.na(A$nonfatal2017)]<-0
stat.desc(A$fatal2017)
stat.desc(A$nonfatal2017)
A$total_2017<-A$fatal2017+A$nonfatal2017
stat.desc(A$total_2017)

A$fatal2018[is.na(A$fatal2018)]<-0
A$nonfatal2018[is.na(A$nonfatal2018)]<-0
stat.desc(A$fatal2018)
stat.desc(A$nonfatal2018)
A$total_2018<-A$fatal2018+A$nonfatal2018
stat.desc(A$total_2018)

A$fatal2019[is.na(A$fatal2019)]<-0
A$nonfatal2019[is.na(A$nonfatal2019)]<-0
stat.desc(A$fatal2019)
stat.desc(A$nonfatal2019)
A$total_2019<-A$fatal2019+A$nonfatal2019
stat.desc(A$total_2019)

A$fatal2020[is.na(A$fatal2020)]<-0
A$nonfatal2020[is.na(A$nonfatal2020)]<-0
stat.desc(A$fatal2020)
stat.desc(A$nonfatal2020)
A$total_2020<-A$fatal2020+A$nonfatal2020
stat.desc(A$total_2020)

A$fatal2021[is.na(A$fatal2021)]<-0
A$nonfatal2021[is.na(A$nonfatal2021)]<-0
stat.desc(A$fatal2021)
stat.desc(A$nonfatal2021)
A$total_2021<-A$fatal2021+A$nonfatal2021
stat.desc(A$total_2021)

A$fatal2022[is.na(A$fatal2022)]<-0
A$nonfatal2022[is.na(A$nonfatal2022)]<-0
stat.desc(A$fatal2022)
stat.desc(A$nonfatal2022)
A$total_2022<-A$fatal2022+A$nonfatal2022
stat.desc(A$total_2022)
## Creating time varying Community Measures ####
names(A)

## Need percent female headed households, percent in poverty, percent unemployed 
names(A)

### 2014 ####

table(A$pctFemHeadHouseholds_2014)
table(A$PovertyRate_2014)
table(A$Unemployed_2014)

A$CDIS_2014<-rowMeans(data.frame(A$pctFemHeadHouseholds_2014,A$PovertyRate_2014,A$Unemployed_2014), na.rm=T)
summary(A$CDIS_2014)



### 2015 ####

table(A$pctFemHeadHouseholds_2015)
table(A$PovertyRate_2015)
table(A$Unemployed_2015)

A$CDIS_2015<-rowMeans(data.frame(A$pctFemHeadHouseholds_2015,A$PovertyRate_2015,A$Unemployed_2015), na.rm=T)
summary(A$CDIS_2015)

### 2016 ####

table(A$pctFemHeadHouseholds_2016)
table(A$PovertyRate_2016)
table(A$Unemployed_2016)

A$CDIS_2016<-rowMeans(data.frame(A$pctFemHeadHouseholds_2016,A$PovertyRate_2016,A$Unemployed_2016), na.rm=T)
summary(A$CDIS_2016)

### 2017 ####

table(A$pctFemHeadHouseholds_2017)
table(A$PovertyRate_2017)
table(A$Unemployed_2017)

A$CDIS_2017<-rowMeans(data.frame(A$pctFemHeadHouseholds_2017,A$PovertyRate_2017,A$Unemployed_2017), na.rm=T)
summary(A$CDIS_2017)

### 2018 ####

table(A$pctFemHeadHouseholds_2018)
table(A$PovertyRate_2018)
table(A$Unemployed_2018)

A$CDIS_2018<-rowMeans(data.frame(A$pctFemHeadHouseholds_2018,A$PovertyRate_2018,A$Unemployed_2018), na.rm=T)
summary(A$CDIS_2018)

### 2019 ####

table(A$pctFemHeadHouseholds_2019)
table(A$PovertyRate_2019)
table(A$Unemployed_2019)

A$CDIS_2019<-rowMeans(data.frame(A$pctFemHeadHouseholds_2019,A$PovertyRate_2019,A$Unemployed_2019), na.rm=T)
summary(A$CDIS_2019)

### 2020 ####

table(A$pctFemHeadHouseholds_2020)
table(A$PovertyRate_2020)
table(A$Unemployed_2020)

A$CDIS_2020<-rowMeans(data.frame(A$pctFemHeadHouseholds_2020,A$PovertyRate_2020,A$Unemployed_2020), na.rm=T)
summary(A$CDIS_2020)

### 2021 ####

table(A$pctFemHeadHouseholds_2021)
table(A$PovertyRate_2021)
table(A$Unemployed_2021)

A$CDIS_2021<-rowMeans(data.frame(A$pctFemHeadHouseholds_2021,A$PovertyRate_2021,A$Unemployed_2021), na.rm=T)
summary(A$CDIS_2021)

### 2022 ####

table(A$pctFemHeadHouseholds_2022)
table(A$PovertyRate_2022)
table(A$Unemployed_2022)

A$CDIS_2022<-rowMeans(data.frame(A$pctFemHeadHouseholds_2022,A$PovertyRate_2022,A$Unemployed_2022), na.rm=T)
summary(A$CDIS_2022)


## Proportion Race Measures ####
names(A)
### 2014 ####
A$prowhite_2014<-(A$nhwhite_2014/A$totalpop_2014)
A$prohispanic_2014<-(A$hispanic_2014/A$totalpop_2014)
A$problack_2014<-(A$nhblack_2014/A$totalpop_2014)
summary(A$prowhite_2014)
summary(A$prohispanic_2014)
summary(A$problack_2014)
### 2015 ####
A$prowhite_2015<-(A$nhwhite_2015/A$totalpop_2015)
A$prohispanic_2015<-(A$hispanic_2015/A$totalpop_2015)
A$problack_2015<-(A$nhblack_2015/A$totalpop_2015)
summary(A$prowhite_2015)
summary(A$prohispanic_2015)
summary(A$problack_2015)

### 2016 ####
A$prowhite_2016<-(A$nhwhite_2016/A$totalpop_2016)
A$prohispanic_2016<-(A$hispanic_2016/A$totalpop_2016)
A$problack_2016<-(A$nhblack_2016/A$totalpop_2016)
summary(A$prowhite_2016)
summary(A$prohispanic_2016)
summary(A$problack_2016)

### 2017 ####
A$prowhite_2017<-(A$nhwhite_2017/A$totalpop_2017)
A$prohispanic_2017<-(A$hispanic_2017/A$totalpop_2017)
A$problack_2017<-(A$nhblack_2017/A$totalpop_2017)
summary(A$prowhite_2017)
summary(A$prohispanic_2017)
summary(A$problack_2017)


### 2018 ####
A$prowhite_2018<-(A$nhwhite_2018/A$totalpop_2018)
A$prohispanic_2018<-(A$hispanic_2018/A$totalpop_2018)
A$problack_2018<-(A$nhblack_2018/A$totalpop_2018)
summary(A$prowhite_2018)
summary(A$prohispanic_2018)
summary(A$problack_2018)

### 2019 ####
A$prowhite_2019<-(A$nhwhite_2019/A$totalpop_2019)
A$prohispanic_2019<-(A$hispanic_2019/A$totalpop_2019)
A$problack_2019<-(A$nhblack_2019/A$totalpop_2019)
summary(A$prowhite_2019)
summary(A$prohispanic_2019)
summary(A$problack_2019)

### 2020 ####
A$prowhite_2020<-(A$nhwhite_2020/A$totalpop_2020)
A$prohispanic_2020<-(A$hispanic_2020/A$totalpop_2020)
A$problack_2020<-(A$nhblack_2020/A$totalpop_2020)
summary(A$prowhite_2020)
summary(A$prohispanic_2020)
summary(A$problack_2020)

### 2021 ####
A$prowhite_2021<-(A$nhwhite_2021/A$totalpop_2021)
A$prohispanic_2021<-(A$hispanic_2021/A$totalpop_2021)
A$problack_2021<-(A$nhblack_2021/A$totalpop_2021)
summary(A$prowhite_2021)
summary(A$prohispanic_2021)
summary(A$problack_2021)
### 2022 ####
A$prowhite_2022<-(A$nhwhite_2022/A$totalpop_2022)
A$prohispanic_2022<-(A$hispanic_2022/A$totalpop_2022)
A$problack_2022<-(A$nhblack_2022/A$totalpop_2022)
summary(A$prowhite_2022)
summary(A$prohispanic_2022)
summary(A$problack_2022)

## Male Female Sex Ratio (14-19) ####
names(A)

### 2014
A$f_m_sexratio_2014<-A$female_2014/A$male_2014
A$f_m_sexratio_2014[A$f_m_sexratio_2014==Inf]<-NA
summary(A$f_m_sexratio_2014)

### 2015
A$f_m_sexratio_2015<-A$female_2015/A$male_2015
A$f_m_sexratio_2015[A$f_m_sexratio_2015==Inf]<-NA
summary(A$f_m_sexratio_2015)

### 2016
A$f_m_sexratio_2016<-A$female_2016/A$male_2016
A$f_m_sexratio_2016[A$f_m_sexratio_2016==Inf]<-NA
summary(A$f_m_sexratio_2016)

### 2017
A$f_m_sexratio_2017<-A$female_2017/A$male_2017
A$f_m_sexratio_2017[A$f_m_sexratio_2017==Inf]<-NA
summary(A$f_m_sexratio_2017)

### 2018
A$f_m_sexratio_2018<-A$female_2018/A$male_2018
A$f_m_sexratio_2018[A$f_m_sexratio_2018==Inf]<-NA
summary(A$f_m_sexratio_2018)

### 2019
A$f_m_sexratio_2019<-A$female_2019/A$male_2019
A$f_m_sexratio_2019[A$f_m_sexratio_2019==Inf]<-NA
summary(A$f_m_sexratio_2019)

### 2020
A$f_m_sexratio_2020<-A$female_2020/A$male_2020
A$f_m_sexratio_2020[A$f_m_sexratio_2020==Inf]<-NA
summary(A$f_m_sexratio_2020)

### 2021
A$f_m_sexratio_2021<-A$female_2021/A$male_2021
A$f_m_sexratio_2021[A$f_m_sexratio_2021==Inf]<-NA
summary(A$f_m_sexratio_2021)

### 2022
A$f_m_sexratio_2022<-A$female_2022/A$male_2022
A$f_m_sexratio_2022[A$f_m_sexratio_2022==Inf]<-NA
summary(A$f_m_sexratio_2022)

## Gun Store Opening & Closing ####
### 2015 ####
table(A$FFLs_2014, useNA = "always")

A$GunS_Open_2015<-NA
A$GunS_Open_2015[A$FFLs_2014 == 0 & A$FFLs_2015 >= 1]<-1
table(A$GunS_Open_2015)
table(A$GunS_Open_2015,A$FFLs_2014)
A$GunS_Open_2015[A$FFLs_2014 >=1|A$FFLs_2015 == 0]<-0
table(A$GunS_Open_2015, useNA = "always")

A$GunS_Closed_2015<-NA
A$GunS_Closed_2015[A$FFLs_2014 >= 1 & A$FFLs_2015 ==0]<-1
table(A$GunS_Closed_2015)
table(A$GunS_Closed_2015,A$FFLs_2014)
A$GunS_Closed_2015[A$FFLs_2014 == 0|A$FFLs_2015 >=1]<-0
table(A$GunS_Closed_2015)
table(A$GunS_Closed_2015,A$FFLs_2014, useNA = "always")

### 2016 ####
table(A$FFLs_2014, useNA = "always")

A$GunS_Open_2016<-NA
A$GunS_Open_2016[A$FFLs_2015 == 0 & A$FFLs_2016 >= 1]<-1
table(A$GunS_Open_2016)
table(A$GunS_Open_2016,A$FFLs_2015)
A$GunS_Open_2016[A$FFLs_2015 >=1|A$FFLs_2016 == 0]<-0
table(A$GunS_Open_2016, useNA = "always")

A$GunS_Closed_2016<-NA
A$GunS_Closed_2016[A$FFLs_2015 >= 1 & A$FFLs_2016 ==0]<-1
table(A$GunS_Closed_2016)
table(A$GunS_Closed_2016,A$FFLs_2015)
A$GunS_Closed_2016[A$FFLs_2015 == 0|A$FFLs_2016 >=1]<-0
table(A$GunS_Closed_2016)
table(A$GunS_Closed_2016,A$FFLs_2015, useNA = "always")

### 2017 ####
table(A$FFLs_2016, useNA = "always")

A$GunS_Open_2017<-NA
A$GunS_Open_2017[A$FFLs_2016 == 0 & A$FFLs_2017 >= 1]<-1
table(A$GunS_Open_2017)
table(A$GunS_Open_2017,A$FFLs_2016)
A$GunS_Open_2017[A$FFLs_2016 >=1|A$FFLs_2017 == 0]<-0
table(A$GunS_Open_2017, useNA = "always")

A$GunS_Closed_2017<-NA
A$GunS_Closed_2017[A$FFLs_2016 >= 1 & A$FFLs_2017 ==0]<-1
table(A$GunS_Closed_2017)
table(A$GunS_Closed_2017,A$FFLs_2016)
A$GunS_Closed_2017[A$FFLs_2016 == 0|A$FFLs_2017 >=1]<-0
table(A$GunS_Closed_2017)
table(A$GunS_Closed_2017,A$FFLs_2016, useNA = "always")

### 2018 ####
table(A$FFLs_2017, useNA = "always")

A$GunS_Open_2018<-NA
A$GunS_Open_2018[A$FFLs_2017 == 0 & A$FFLs_2018 >= 1]<-1
table(A$GunS_Open_2018)
table(A$GunS_Open_2018,A$FFLs_2017)
A$GunS_Open_2018[A$FFLs_2017 >=1|A$FFLs_2018 == 0]<-0
table(A$GunS_Open_2018, useNA = "always")

A$GunS_Closed_2018<-NA
A$GunS_Closed_2018[A$FFLs_2017 >= 1 & A$FFLs_2018 ==0]<-1
table(A$GunS_Closed_2018)
table(A$GunS_Closed_2018,A$FFLs_2017)
A$GunS_Closed_2018[A$FFLs_2017 == 0|A$FFLs_2018 >=1]<-0
table(A$GunS_Closed_2018)
table(A$GunS_Closed_2018,A$FFLs_2017, useNA = "always")

### 2019 ####
table(A$FFLs_2018, useNA = "always")

A$GunS_Open_2019<-NA
A$GunS_Open_2019[A$FFLs_2018 == 0 & A$FFLs_2019 >= 1]<-1
table(A$GunS_Open_2019)
table(A$GunS_Open_2019,A$FFLs_2018)
A$GunS_Open_2019[A$FFLs_2018 >=1|A$FFLs_2019 == 0]<-0
table(A$GunS_Open_2019, useNA = "always")

A$GunS_Closed_2019<-NA
A$GunS_Closed_2019[A$FFLs_2018 >= 1 & A$FFLs_2019 ==0]<-1
table(A$GunS_Closed_2019)
table(A$GunS_Closed_2019,A$FFLs_2018)
A$GunS_Closed_2019[A$FFLs_2018 == 0|A$FFLs_2019 >=1]<-0
table(A$GunS_Closed_2019)
table(A$GunS_Closed_2019,A$FFLs_2018, useNA = "always")

### 2020 ####
table(A$FFLs_2019, useNA = "always")

A$GunS_Open_2020<-NA
A$GunS_Open_2020[A$FFLs_2019 == 0 & A$FFLs_2020 >= 1]<-1
table(A$GunS_Open_2020)
table(A$GunS_Open_2020,A$FFLs_2019)
A$GunS_Open_2020[A$FFLs_2019 >=1|A$FFLs_2020 == 0]<-0
table(A$GunS_Open_2020, useNA = "always")

A$GunS_Closed_2020<-NA
A$GunS_Closed_2020[A$FFLs_2019 >= 1 & A$FFLs_2020 ==0]<-1
table(A$GunS_Closed_2020)
table(A$GunS_Closed_2020,A$FFLs_2019)
A$GunS_Closed_2020[A$FFLs_2019 == 0|A$FFLs_2020 >=1]<-0
table(A$GunS_Closed_2020)
table(A$GunS_Closed_2020,A$FFLs_2019, useNA = "always")

### 2021 ####
table(A$FFLs_2020, useNA = "always")

A$GunS_Open_2021<-NA
A$GunS_Open_2021[A$FFLs_2020 == 0 & A$FFLs_2021 >= 1]<-1
table(A$GunS_Open_2021)
table(A$GunS_Open_2021,A$FFLs_2020)
A$GunS_Open_2021[A$FFLs_2020 >=1|A$FFLs_2021 == 0]<-0
table(A$GunS_Open_2021, useNA = "always")

A$GunS_Closed_2021<-NA
A$GunS_Closed_2021[A$FFLs_2020 >= 1 & A$FFLs_2021 ==0]<-1
table(A$GunS_Closed_2021)
table(A$GunS_Closed_2021,A$FFLs_2020)
A$GunS_Closed_2021[A$FFLs_2020 == 0|A$FFLs_2021 >=1]<-0
table(A$GunS_Closed_2021)
table(A$GunS_Closed_2021,A$FFLs_2020, useNA = "always")

### 2022 ####
table(A$FFLs_2021, useNA = "always")

A$GunS_Open_2022<-NA
A$GunS_Open_2022[A$FFLs_2021 == 0 & A$FFLs_2022 >= 1]<-1
table(A$GunS_Open_2022)
table(A$GunS_Open_2022,A$FFLs_2021)
A$GunS_Open_2022[A$FFLs_2021 >=1|A$FFLs_2022 == 0]<-0
table(A$GunS_Open_2022, useNA = "always")

A$GunS_Closed_2022<-NA
A$GunS_Closed_2022[A$FFLs_2021 >= 1 & A$FFLs_2022 ==0]<-1
table(A$GunS_Closed_2022)
table(A$GunS_Closed_2022,A$FFLs_2021)
A$GunS_Closed_2022[A$FFLs_2021 == 0|A$FFLs_2022 >=1]<-0
table(A$GunS_Closed_2022)
table(A$GunS_Closed_2022,A$FFLs_2021, useNA = "always")

# Gun Store Descriptives ####

sink("Results/Gun Store Open_Closing Descriptives.txt")
print("gun store openings 2015")
table(A$GunS_Open_2015)
print("gun store Closings 2015")
table(A$GunS_Closed_2015)
print("gun store openings 2016")
table(A$GunS_Open_2016)
print("gun store Closings 2016")
table(A$GunS_Closed_2016)
print("gun store openings 2017")
table(A$GunS_Open_2017)
print("gun store Closings 2017")
table(A$GunS_Closed_2017)
print("gun store openings 2018")
table(A$GunS_Open_2018)
print("gun store Closings 2018")
table(A$GunS_Closed_2018)
print("gun store openings 2019")
table(A$GunS_Open_2019)
print("gun store Closings 2019")
table(A$GunS_Closed_2019)
print("gun store openings 2020")
table(A$GunS_Open_2020)
print("gun store Closings 2020")
table(A$GunS_Closed_2020)
print("gun store openings 2021")
table(A$GunS_Open_2021)
print("gun store Closings 2021")
table(A$GunS_Closed_2021)
print("gun store openings 2022")
table(A$GunS_Open_2022)
print("gun store Closings 2022")
table(A$GunS_Closed_2022)
sink()
# Limiting Data Frame ####
names(A)

B<-A[,c("gisjoin","geoid_2020","total_2015","total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022",
        "fatal2015","fatal2016","fatal2017","fatal2018","fatal2019","fatal2020","fatal2021","fatal2022",
        "nonfatal2015","nonfatal2016","nonfatal2017","nonfatal2018","nonfatal2019","nonfatal2020","nonfatal2021","nonfatal2022",
        "CDIS_2015","CDIS_2016","CDIS_2017","CDIS_2018","CDIS_2019","CDIS_2020","CDIS_2021","CDIS_2022",
       "prowhite_2015","prohispanic_2015",
        "problack_2015","prowhite_2016","prohispanic_2016","problack_2016",
        "prowhite_2017","prohispanic_2017","problack_2017","prowhite_2018",
        "prohispanic_2018","problack_2018","prowhite_2019","prohispanic_2019",
        "problack_2019","prowhite_2020","prohispanic_2020","problack_2020",
        "prowhite_2021","prohispanic_2021","problack_2021","prowhite_2022",
        "prohispanic_2022","problack_2022",
        "f_m_sexratio_2015","f_m_sexratio_2016","f_m_sexratio_2017",
        "f_m_sexratio_2018","f_m_sexratio_2019","f_m_sexratio_2020",
        "f_m_sexratio_2021","f_m_sexratio_2022",
        "pctYoungMale1524_2015","pctLivingAlone_2015","PublicAssist_2015","totalpop_2015",
        "RentBurden_2015",
        "pctYoungMale1524_2016","pctLivingAlone_2016","PublicAssist_2016","totalpop_2016",
        "RentBurden_2016",
        "pctYoungMale1524_2017","PctLivingAlone_2017","PublicAssist_2017","totalpop_2017",
        "RentBurden_2017",
        "pctYoungMale1524_2018","pctLivingAlone_2018","PublicAssist_2018","totalpop_2018",
        "RentBurden_2018",
        "pctYoungMale1524_2019","pctLivingAlone_2019","PublicAssist_2019","totalpop_2019",
        "RentBurden_2019",
        "pctYoungMale1524_2020","pctLivingAlone_2020","PublicAssist_2020","totalpop_2020",
        "RentBurden_2020",
        "pctYoungMale1524_2021","pctLivingAlone_2021","PublicAssist_2021","totalpop_2021",
        "RentBurden_2021",
        "pctYoungMale1524_2022","pctLivingAlone_2022","PublicAssist_2022","totalpop_2022",
        "RentBurden_2022",
        "GunS_Open_2015","GunS_Closed_2015",
        "GunS_Open_2016","GunS_Closed_2016",
        "GunS_Open_2017","GunS_Closed_2017",
        "GunS_Open_2018","GunS_Closed_2018",
        "GunS_Open_2019","GunS_Closed_2019",
        "GunS_Open_2020","GunS_Closed_2020",
        "GunS_Open_2021","GunS_Closed_2021",
        "GunS_Open_2022","GunS_Closed_2022")]
# Wide Descriptive Statistics: Pre-Imputation ####
names(B)

sink("Results/pre-Imputation Descriptives (Wide).txt")
stat.desc(B)
sink()


# Multiple Imputation (Start here) ####
# Missing Case Analysis ####


Z<-B[complete.cases(B),]
names(Z)

Z$CCDI<-1

Z.1<-Z[, c("geoid_2020","CCDI")]


Z.1<-Z.1[, c("geoid_2020","CCDI")]

MCDF<-merge(B, Z.1, by = c("geoid_2020"), all.x = T)

MCDF$CCDI<-as.numeric(MCDF$CCDI)

table(MCDF$CCDI, useNA="always")

MCDF$CCDI[is.na(MCDF$CCDI)]<-0

table(MCDF$CCDI, useNA="always")

nlevels(MCDF$CCDI)

library(finalfit)

jpeg("Results/Missing Values Plot.jpeg", width = 500, height = 500, quality = 100)
missing_plot(MCDF)
dev.off()

jpeg("Results/Missing Pattern Plot.jpeg", width = 1000, height = 1000, quality = 100)
missing_pattern(MCDF)
dev.off()

sink("Results/Missing Pattern Results.txt")
missing_pattern(MCDF)
sink()


names(MCDF)

sink("Results/Missing Case Analysis (t_test).txt")
stat.desc(MCDF[which(MCDF$CCDI == 1),])
stat.desc(MCDF[which(MCDF$CCDI == 0),])
print("# t-test")
print("# Complete Cases (0 = incomeplete cases; 1 = complete cases)")
table(MCDF$CCDI)
lapply(MCDF[,c(3:122)], function(x, na.rm = FALSE) t.test(x~MCDF$CCDI))
sink()



sink("Results/Missing Case Analysis (Cohen's d).txt")
print("# Year_1")
print("# Complete Cases (0 = incomeplete cases; 1 = complete cases)")
table(MCDF$CCDI)
lapply(MCDF[,c(3:122)], function(x, na.rm = FALSE) cohen.d(x~MCDF$CCDI))
sink()



# Imputing Data ####
names(B)
stat.desc(B)


IM_E<-mice(B, m=25, maxit = 1, method = c("rf"), seed = 1992)


IM_F<-complete(IM_E)

stat.desc(IM_F)

fwrite(IM_F,"Data/Imputed Analytical Data.csv", buffMB = 1024L, append = F, sep = ",", quote = F, compress = "none")  


# Loading Imputed Data (USE ME) ####
G<-fread("Data/Imputed Analytical Data.csv", sep = ",", fill=TRUE, quote=F, na.strings="NA")
# Wide Descriptive Statistics: Post-Imputation ####
names(G)

sink("Results/post-Imputation Descriptives (Wide).txt")
stat.desc(G)
sink()


# Creating Long Dataframe ####
names(G)

## Dependent Variables of Interest ####

test<-G[!duplicated(G[, c("geoid_2020")]), ]


DF1<-reshape(G, varying =c("total_2015","total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total15_22", timevar = "Year", direction = "long")

names(DF1)
DF1<-(DF1[, c("geoid_2020","total15_22","Year")])


DF2<-reshape(G, varying =c("total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total16_22", timevar = "Year", direction = "long")

names(DF2)
DF2<-(DF2[, c("geoid_2020","total16_22","Year")])

## independent Variables of Interest ####
names(G)

DF3<-reshape(G, varying =c("GunS_Open_2015","GunS_Open_2016","GunS_Open_2017","GunS_Open_2018","GunS_Open_2019","GunS_Open_2020","GunS_Open_2021","GunS_Open_2022"), 
             v.names = "GunS_Open15_22", timevar = "Year", direction = "long")

names(DF3)
DF3<-(DF3[, c("geoid_2020","GunS_Open15_22","Year")])


DF4<-reshape(G, varying =c("GunS_Open_2015","GunS_Open_2016","GunS_Open_2017","GunS_Open_2018","GunS_Open_2019","GunS_Open_2020","GunS_Open_2021"), 
             v.names = "GunS_Open15_21", timevar = "Year", direction = "long")

names(DF4)
DF4<-(DF4[, c("geoid_2020","GunS_Open15_21","Year")])


names(G)

DF5<-reshape(G, varying =c("GunS_Closed_2015","GunS_Closed_2016","GunS_Closed_2017","GunS_Closed_2018","GunS_Closed_2019","GunS_Closed_2020","GunS_Closed_2021","GunS_Closed_2022"), 
             v.names = "GunS_Closed15_22", timevar = "Year", direction = "long")

names(DF5)
DF5<-(DF5[, c("geoid_2020","GunS_Closed15_22","Year")])


DF6<-reshape(G, varying =c("GunS_Closed_2015","GunS_Closed_2016","GunS_Closed_2017","GunS_Closed_2018","GunS_Closed_2019","GunS_Closed_2020","GunS_Closed_2021"), 
             v.names = "GunS_Closed15_21", timevar = "Year", direction = "long")

names(DF6)
DF6<-(DF6[, c("geoid_2020","GunS_Closed15_21","Year")])

## Covariates of Interest ####

names(G)


G <- G %>% 
  rename(
    pctLivingAlone_2017 = PctLivingAlone_2017
  )


long_data <- pivot_longer(
  data = G,
  cols = starts_with(c("prowhite_", "problack_","prohispanic_","f_m_sexratio_",
                       "pctYoungMale1524_","pctLivingAlone_","PublicAssist_",
                       "totalpop_",
                       "RentBurden_","CDIS_")),  
  names_to = c(".value", "year"),  # Specify the new column names
  names_pattern = "(.*)_(\\d+)"    # Define pattern to extract variable names and years
)

names(long_data)

names(long_data)
long_data<-(long_data[, c("geoid_2020","year","prowhite",                
                          "problack","prohispanic","f_m_sexratio",            
                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                          "totalpop",       
                          "RentBurden","CDIS")])

long_data$Year<-NA
long_data$Year[long_data$year==2015]<-1
long_data$Year[long_data$year==2016]<-2
long_data$Year[long_data$year==2017]<-3
long_data$Year[long_data$year==2018]<-4
long_data$Year[long_data$year==2019]<-5
long_data$Year[long_data$year==2020]<-6
long_data$Year[long_data$year==2021]<-7
long_data$Year[long_data$year==2022]<-8

long_data<-data.frame(long_data)

H<-Reduce(function(x, y) merge(x, y, by = c("geoid_2020","Year"), all.x = T),
          list(long_data,DF1,DF2,DF3,DF4,DF5,DF6))

names(H)

table(H$GunS_Open15_21, useNA = "always")

# Descriptive Statistics Long ####
sink("Results/post-Imputation Descriptives (Long).txt")
stat.desc(H)
sink()


#______________________________________________####
# Non-Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(H)

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total15_22","Year","GunS_Open15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreOpen 15_22 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(H)

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total15_22","Year","GunS_Closed15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreClosed 15_22 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

names(H)

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total16_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreOpen 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

names(H)

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total16_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreClosed 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Non-Lagged Models (CDIS High)                ####
#______________________________________________####
# CDIS High Sample 75th Percentile ####
quantile(H$CDIS, c(.10,.25,.50,.75,.90))

J<-H[which(H$CDIS >16.93729682267),]


# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(J)

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total15_22","Year","GunS_Open15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreOpen 15_22 (CDIS High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(J)

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total15_22","Year","GunS_Closed15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreClosed 15_22 (CDIS High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()





# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

names(J)

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total16_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreOpen 15_21 (CDIS High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

names(J)

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total16_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreClosed 15_21 (CDIS High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Non-Lagged Models (CDIS Mod)                ####
#______________________________________________####
# CDIS Mod Sample 25-75th Percentile ####
quantile(H$CDIS, c(.10,.25,.50,.75,.90))

K<-H[which(H$CDIS >=6.61688262224 & H$CDIS <=16.93729682267),]


# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(K)

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(K[, c("total15_22","Year","GunS_Open15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreOpen 15_22 (CDIS Mod).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(K)

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(K[, c("total15_22","Year","GunS_Closed15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreClosed 15_22 (CDIS Mod).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()





# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

names(K)

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(K[, c("total16_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreOpen 15_21 (CDIS Mod).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

names(K)

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(K[, c("total16_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreClosed 15_21 (CDIS Mod).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()



#______________________________________________####
# Non-Lagged Models (CDIS Low)                ####
#______________________________________________####
# CDIS Low Sample 25th Percentile ####
quantile(H$CDIS, c(.10,.25,.50,.75,.90))

L<-H[which(H$CDIS <6.61688262224),]


# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(L)
stat.desc(L)

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(L[, c("total15_22","Year","GunS_Open15_22","prowhite",                
                                          "prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreOpen 15_22 (CDIS Low).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(L)

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(L[, c("total15_22","Year","GunS_Closed15_22","prowhite",                
                                          "prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreClosed 15_22 (CDIS Low).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()





# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

names(L)

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(L[, c("total16_22","Year","GunS_Open15_21","prowhite",                
                                          "prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreOpen 15_21 (CDIS Low).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

names(L)

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(L[, c("total16_22","Year","GunS_Closed15_21","prowhite",                
                                          "prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreClosed 15_21 (CDIS Low).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Non-Lagged Models (CDIS Very High)                ####
#______________________________________________####
# CDIS Very High Sample 90th Percentile ####
quantile(H$CDIS, c(.10,.25,.50,.75,.90))

M<-H[which(H$CDIS >23.37977202733),]


# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(M)

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(M[, c("total15_22","Year","GunS_Open15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreOpen 15_22 (CDIS Very High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(M)

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(M[, c("total15_22","Year","GunS_Closed15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreClosed 15_22 (CDIS Very High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()





# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

names(M)

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(M[, c("total16_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreOpen 15_21 (CDIS Very High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

names(M)

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(M[, c("total16_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreClosed 15_21 (CDIS Very High).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Gun Store Changes (adding or subtracting store)####
#______________________________________________####



# Gun Store Changes ####
## 2015 ####
A$GunS_More_2015<-NA
A$GunS_More_2015[A$FFLs_2015 > A$FFLs_2014]<-1
table(A$GunS_More_2015)
table(A$GunS_More_2015,A$FFLs_2014)
A$GunS_More_2015[A$FFLs_2015<=A$FFLs_2014]<-0
table(A$GunS_More_2015, useNA = "always")

A$GunS_Less_2015<-NA
A$GunS_Less_2015[A$FFLs_2015 < A$FFLs_2014]<-1
table(A$GunS_Less_2015)
table(A$GunS_Less_2015,A$FFLs_2014)
A$GunS_Less_2015[A$FFLs_2015 >= A$FFLs_2014]<-0
table(A$GunS_Less_2015, useNA = "always")
## 2016 ####
A$GunS_More_2016<-NA
A$GunS_More_2016[A$FFLs_2016 > A$FFLs_2015]<-1
table(A$GunS_More_2016)
table(A$GunS_More_2016,A$FFLs_2015)
A$GunS_More_2016[A$FFLs_2016<=A$FFLs_2015]<-0
table(A$GunS_More_2016, useNA = "always")

A$GunS_Less_2016<-NA
A$GunS_Less_2016[A$FFLs_2016 < A$FFLs_2015]<-1
table(A$GunS_Less_2016)
table(A$GunS_Less_2016,A$FFLs_2015)
A$GunS_Less_2016[A$FFLs_2016 >= A$FFLs_2015]<-0
table(A$GunS_Less_2016, useNA = "always")
## 2017 ####
A$GunS_More_2017<-NA
A$GunS_More_2017[A$FFLs_2017 > A$FFLs_2016]<-1
table(A$GunS_More_2017)
table(A$GunS_More_2017,A$FFLs_2016)
A$GunS_More_2017[A$FFLs_2017<=A$FFLs_2016]<-0
table(A$GunS_More_2017, useNA = "always")

A$GunS_Less_2017<-NA
A$GunS_Less_2017[A$FFLs_2017 < A$FFLs_2016]<-1
table(A$GunS_Less_2017)
table(A$GunS_Less_2017,A$FFLs_2016)
A$GunS_Less_2017[A$FFLs_2017 >= A$FFLs_2016]<-0
table(A$GunS_Less_2017, useNA = "always")
## 2018 ####
A$GunS_More_2018<-NA
A$GunS_More_2018[A$FFLs_2018 > A$FFLs_2017]<-1
table(A$GunS_More_2018)
table(A$GunS_More_2018,A$FFLs_2017)
A$GunS_More_2018[A$FFLs_2018<=A$FFLs_2017]<-0
table(A$GunS_More_2018, useNA = "always")

A$GunS_Less_2018<-NA
A$GunS_Less_2018[A$FFLs_2018 < A$FFLs_2017]<-1
table(A$GunS_Less_2018)
table(A$GunS_Less_2018,A$FFLs_2017)
A$GunS_Less_2018[A$FFLs_2018 >= A$FFLs_2017]<-0
table(A$GunS_Less_2018, useNA = "always")
## 2019 ####
A$GunS_More_2019<-NA
A$GunS_More_2019[A$FFLs_2019 > A$FFLs_2018]<-1
table(A$GunS_More_2019)
table(A$GunS_More_2019,A$FFLs_2018)
A$GunS_More_2019[A$FFLs_2019<=A$FFLs_2018]<-0
table(A$GunS_More_2019, useNA = "always")

A$GunS_Less_2019<-NA
A$GunS_Less_2019[A$FFLs_2019 < A$FFLs_2018]<-1
table(A$GunS_Less_2019)
table(A$GunS_Less_2019,A$FFLs_2018)
A$GunS_Less_2019[A$FFLs_2019 >= A$FFLs_2018]<-0
table(A$GunS_Less_2019, useNA = "always")
## 2020 ####
A$GunS_More_2020<-NA
A$GunS_More_2020[A$FFLs_2020 > A$FFLs_2019]<-1
table(A$GunS_More_2020)
table(A$GunS_More_2020,A$FFLs_2019)
A$GunS_More_2020[A$FFLs_2020<=A$FFLs_2019]<-0
table(A$GunS_More_2020, useNA = "always")

A$GunS_Less_2020<-NA
A$GunS_Less_2020[A$FFLs_2020 < A$FFLs_2019]<-1
table(A$GunS_Less_2020)
table(A$GunS_Less_2020,A$FFLs_2019)
A$GunS_Less_2020[A$FFLs_2020 >= A$FFLs_2019]<-0
table(A$GunS_Less_2020, useNA = "always")
## 2021 ####
A$GunS_More_2021<-NA
A$GunS_More_2021[A$FFLs_2021 > A$FFLs_2020]<-1
table(A$GunS_More_2021)
table(A$GunS_More_2021,A$FFLs_2020)
A$GunS_More_2021[A$FFLs_2021<=A$FFLs_2020]<-0
table(A$GunS_More_2021, useNA = "always")

A$GunS_Less_2021<-NA
A$GunS_Less_2021[A$FFLs_2021 < A$FFLs_2020]<-1
table(A$GunS_Less_2021)
table(A$GunS_Less_2021,A$FFLs_2020)
A$GunS_Less_2021[A$FFLs_2021 >= A$FFLs_2020]<-0
table(A$GunS_Less_2021, useNA = "always")
## 2022 ####
A$GunS_More_2022<-NA
A$GunS_More_2022[A$FFLs_2022 > A$FFLs_2021]<-1
table(A$GunS_More_2022)
table(A$GunS_More_2022,A$FFLs_2021)
A$GunS_More_2022[A$FFLs_2022<=A$FFLs_2021]<-0
table(A$GunS_More_2022, useNA = "always")

A$GunS_Less_2022<-NA
A$GunS_Less_2022[A$FFLs_2022 < A$FFLs_2021]<-1
table(A$GunS_Less_2022)
table(A$GunS_Less_2022,A$FFLs_2021)
A$GunS_Less_2022[A$FFLs_2022 >= A$FFLs_2021]<-0
table(A$GunS_Less_2022, useNA = "always")

## Limiting Data #####
names(A)
B<-A[,c("gisjoin","geoid_2020","total_2015","total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022",
        "fatal2015","fatal2016","fatal2017","fatal2018","fatal2019","fatal2020","fatal2021","fatal2022",
        "nonfatal2015","nonfatal2016","nonfatal2017","nonfatal2018","nonfatal2019","nonfatal2020","nonfatal2021","nonfatal2022",
        "CDIS_2015","CDIS_2016","CDIS_2017","CDIS_2018","CDIS_2019","CDIS_2020","CDIS_2021","CDIS_2022",
        "prowhite_2015","prohispanic_2015",
        "problack_2015","prowhite_2016","prohispanic_2016","problack_2016",
        "prowhite_2017","prohispanic_2017","problack_2017","prowhite_2018",
        "prohispanic_2018","problack_2018","prowhite_2019","prohispanic_2019",
        "problack_2019","prowhite_2020","prohispanic_2020","problack_2020",
        "prowhite_2021","prohispanic_2021","problack_2021","prowhite_2022",
        "prohispanic_2022","problack_2022",
        "f_m_sexratio_2015","f_m_sexratio_2016","f_m_sexratio_2017",
        "f_m_sexratio_2018","f_m_sexratio_2019","f_m_sexratio_2020",
        "f_m_sexratio_2021","f_m_sexratio_2022",
        "pctYoungMale1524_2015","pctLivingAlone_2015","PublicAssist_2015","totalpop_2015",
        "RentBurden_2015",
        "pctYoungMale1524_2016","pctLivingAlone_2016","PublicAssist_2016","totalpop_2016",
        "RentBurden_2016",
        "pctYoungMale1524_2017","PctLivingAlone_2017","PublicAssist_2017","totalpop_2017",
        "RentBurden_2017",
        "pctYoungMale1524_2018","pctLivingAlone_2018","PublicAssist_2018","totalpop_2018",
        "RentBurden_2018",
        "pctYoungMale1524_2019","pctLivingAlone_2019","PublicAssist_2019","totalpop_2019",
        "RentBurden_2019",
        "pctYoungMale1524_2020","pctLivingAlone_2020","PublicAssist_2020","totalpop_2020",
        "RentBurden_2020",
        "pctYoungMale1524_2021","pctLivingAlone_2021","PublicAssist_2021","totalpop_2021",
        "RentBurden_2021",
        "pctYoungMale1524_2022","pctLivingAlone_2022","PublicAssist_2022","totalpop_2022",
        "RentBurden_2022",
        "GunS_More_2015","GunS_Less_2015",
        "GunS_More_2016","GunS_Less_2016",
        "GunS_More_2017","GunS_Less_2017",
        "GunS_More_2018","GunS_Less_2018",
        "GunS_More_2019","GunS_Less_2019",
        "GunS_More_2020","GunS_Less_2020",
        "GunS_More_2021","GunS_Less_2021",
        "GunS_More_2022","GunS_Less_2022")]

# Imputing Data ####
names(B)
stat.desc(B)


IM_E<-mice(B, m=25, maxit = 1, method = c("rf"), seed = 1992)


IM_F<-complete(IM_E)

stat.desc(IM_F)

fwrite(IM_F,"Data/Imputed Replication Data.csv", buffMB = 1024L, append = F, sep = ",", quote = F, compress = "none")  
# Loading Imputed Data (USE ME) ####
G<-fread("Data/Imputed Replication Data.csv", sep = ",", fill=TRUE, quote=F, na.strings="NA")
# Creating Long Dataframe ####
names(G)

## Dependent Variables of Interest ####

test<-G[!duplicated(G[, c("geoid_2020")]), ]


DF1<-reshape(G, varying =c("total_2015","total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total15_22", timevar = "Year", direction = "long")

names(DF1)
DF1<-(DF1[, c("geoid_2020","total15_22","Year")])


DF2<-reshape(G, varying =c("total_2016","total_2017","total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total16_22", timevar = "Year", direction = "long")

names(DF2)
DF2<-(DF2[, c("geoid_2020","total16_22","Year")])

## independent Variables of Interest ####
names(G)

DF3<-reshape(G, varying =c("GunS_More_2015","GunS_More_2016","GunS_More_2017","GunS_More_2018","GunS_More_2019","GunS_More_2020","GunS_More_2021","GunS_More_2022"), 
             v.names = "GunS_More15_22", timevar = "Year", direction = "long")

names(DF3)
DF3<-(DF3[, c("geoid_2020","GunS_More15_22","Year")])


DF4<-reshape(G, varying =c("GunS_More_2015","GunS_More_2016","GunS_More_2017","GunS_More_2018","GunS_More_2019","GunS_More_2020","GunS_More_2021"), 
             v.names = "GunS_More15_21", timevar = "Year", direction = "long")

names(DF4)
DF4<-(DF4[, c("geoid_2020","GunS_More15_21","Year")])


names(G)

DF5<-reshape(G, varying =c("GunS_Less_2015","GunS_Less_2016","GunS_Less_2017","GunS_Less_2018","GunS_Less_2019","GunS_Less_2020","GunS_Less_2021","GunS_Less_2022"), 
             v.names = "GunS_Less15_22", timevar = "Year", direction = "long")

names(DF5)
DF5<-(DF5[, c("geoid_2020","GunS_Less15_22","Year")])


DF6<-reshape(G, varying =c("GunS_Less_2015","GunS_Less_2016","GunS_Less_2017","GunS_Less_2018","GunS_Less_2019","GunS_Less_2020","GunS_Less_2021"), 
             v.names = "GunS_Less15_21", timevar = "Year", direction = "long")

names(DF6)
DF6<-(DF6[, c("geoid_2020","GunS_Less15_21","Year")])

## Covariates of Interest ####

names(G)


G <- G %>% 
  rename(
    pctLivingAlone_2017 = PctLivingAlone_2017
  )


long_data <- pivot_longer(
  data = G,
  cols = starts_with(c("prowhite_", "problack_","prohispanic_","f_m_sexratio_",
                       "pctYoungMale1524_","pctLivingAlone_","PublicAssist_",
                       "totalpop_",
                       "RentBurden_","CDIS_")),  
  names_to = c(".value", "year"),  # Specify the new column names
  names_pattern = "(.*)_(\\d+)"    # Define pattern to extract variable names and years
)

names(long_data)

names(long_data)
long_data<-(long_data[, c("geoid_2020","year","prowhite",                
                          "problack","prohispanic","f_m_sexratio",            
                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                          "totalpop",       
                          "RentBurden","CDIS")])

long_data$Year<-NA
long_data$Year[long_data$year==2015]<-1
long_data$Year[long_data$year==2016]<-2
long_data$Year[long_data$year==2017]<-3
long_data$Year[long_data$year==2018]<-4
long_data$Year[long_data$year==2019]<-5
long_data$Year[long_data$year==2020]<-6
long_data$Year[long_data$year==2021]<-7
long_data$Year[long_data$year==2022]<-8

long_data<-data.frame(long_data)

H<-Reduce(function(x, y) merge(x, y, by = c("geoid_2020","Year"), all.x = T),
          list(long_data,DF1,DF2,DF3,DF4,DF5,DF6))

names(H)

table(H$GunS_More15_22, useNA = "always")



# Descriptive Statistics Long ####
sink("Results/post-Imputation Replication Descriptives (Long).txt")
stat.desc(H)
sink()

# Total Gun Violence 15-22 on Gun Stores More 15-22 ####

names(H)

M1<-gamlss(total15_22~GunS_More15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total15_22","Year","GunS_More15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreMore 15_22 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Less 15-22 ####

names(H)

M1<-gamlss(total15_22~GunS_Less15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total15_22","Year","GunS_Less15_22","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 15_22 GunStoreLess 15_22 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 16-22 on Gun Stores More 15-21 ####

names(H)

M1<-gamlss(total16_22~GunS_More15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total16_22","Year","GunS_More15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreMore 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Less 15-21 ####

names(H)

M1<-gamlss(total16_22~GunS_Less15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(H[, c("total16_22","Year","GunS_Less15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 16_22 GunStoreLess 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()



#______________________________________________####
# Gun Store Changes (Longer Lags 2 years + 3 years)####
#______________________________________________####


test<-G[!duplicated(G[, c("geoid_2020")]), ]


DF7<-reshape(G, varying =c("total_2017","total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total17_22", timevar = "Year", direction = "long")

names(DF7)
DF7<-(DF7[, c("geoid_2020","total17_22","Year")])

DF8<-reshape(G, varying =c("total_2018","total_2019","total_2020","total_2021","total_2022"), 
             v.names = "total18_22", timevar = "Year", direction = "long")

names(DF8)
DF8<-(DF8[, c("geoid_2020","total18_22","Year")])


J<-Reduce(function(x, y) merge(x, y, by = c("geoid_2020","Year"), all.x = T),
          list(H,DF7,DF8))

names(J)



J2<-J[which(!is.na(J$total17_22)),]
J3<-J[which(!is.na(J$total18_22)),]

sink("Results/post-Imputation Descriptives (Long; Only Lagg).txt")
stat.desc(J[which(!is.na(J$total16_22)),])
stat.desc(J2$GunS_Open15_21)
stat.desc(J2$GunS_Closed15_21)
stat.desc(J3$GunS_Open15_21)
stat.desc(J3$GunS_Closed15_21)
sink()



# Total Gun Violence 17-22 on Gun Stores Open 15-21 ####

names(J)

M1<-gamlss(total17_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total17_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 17_22 GunStoreOpen 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 17-22 on Gun Stores Closed 15-21 ####
names(J)

M1<-gamlss(total17_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total17_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 17_22 GunStoreClosed 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 18-22 on Gun Stores Open 15-21 ####

names(J)

M1<-gamlss(total18_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total18_22","Year","GunS_Open15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 18_22 GunStoreOpen 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 18-22 on Gun Stores Closed 15-21 ####

names(J)

M1<-gamlss(total18_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=na.omit(J[, c("total18_22","Year","GunS_Closed15_21","prowhite",                
                                          "problack","prohispanic","f_m_sexratio",            
                                          "pctYoungMale1524","pctLivingAlone","PublicAssist",            
                                          "totalpop",       
                                          "RentBurden","CDIS","geoid_2020")]),
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Results/Total 18_22 GunStoreClosed 15_21 (controls included).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


#______________________________________________####
# Figure                                       ####
#______________________________________________####

DV<-c("Total Shootings (16-22) - One Year Lag","Total Shootings (16-22) - One Year Lag",
      "Total Shootings (17-22) - Two Year Lag","Total Shootings (17-22) - Two Year Lag",
      "Total Shootings (18-22) - Three Year Lag","Total Shootings (18-22) - Three Year Lag") # Heading for Facet Wrap
IV<-c("Gun Store Opens","Gun Stores Close") # Independent variable names
ES<-c(-0.018204314363190,0.06054778202326,
      0.0743286602937646,-0.0681161351109950,
      0.08524363067694832,0.002495338289266403) # b Estimate (could be standardized estimate, Odds Ratio, Incident Rate Ratio, etc.)
LCI<-c(-0.06005100998006990,0.01610563550516357,
       0.0197447506777388,-0.11522658978306853,
       0.02302162539383838,-0.07229678080490774) # Lower 95% confidence interval
UCI<-c(0.0236423812536896,0.1049899285413610,
       0.1289125699097902,-0.0210056804389215,
       0.14746563596005827,0.07728745738344055) # Upper 95% confidence interval

A<-data.frame(DV,IV,ES,LCI,UCI)

A$IV<-factor(A$IV, levels=c("Gun Stores Close","Gun Store Opens"))

A$DV<-factor(A$DV, levels=c("Total Shootings (16-22) - One Year Lag",
                            "Total Shootings (17-22) - Two Year Lag",
                            "Total Shootings (18-22) - Three Year Lag"))

jpeg("Results/slope coefficients for Models.jpeg", width = 3000, height = 3000, quality = 100)
ggplot(data=A, aes(x=IV, y=ES, ymin=LCI, ymax=UCI)) +
  geom_pointrange()+ # Makes range for ggplot values based on the data and AES specified in first line
  geom_hline(yintercept=0, lty=2, size =3) +  # add a dotted line at x=0 after flip
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.2, cex=3)+ # Makes whiskers on the range (more aesthetically pleasing)
  facet_wrap(~DV,nrow=3,scales = "free_x")+ # Makes DV header (Can handle multiple DVs)
  coord_flip() + # flip coordinates (puts labels on y axis)
  geom_point(shape = 15, size = 6) + # specifies the size and shape of the geompoint
  ggtitle("")+ # Blank Title for the Graph
  xlab("") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("IRR (95% CI)") + # Label on the X axis (flipped specification do to coord_flip)
  scale_y_continuous(limits = c(-.250,.250), breaks = c(-.250,-.125,0,.125,.250))+ # limits and tic marks on X axis (flipped specification do to coord_flip)
  theme(line = element_line(colour = "black", size = 1), # My personal theme for GGplots
        strip.background = element_rect(fill="gray90"),
        strip.text.x = element_text(family="Times New Roman",size=48, color = "Black"),
        legend.position ="none", 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_blank(), 
        panel.border= element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"), # added to theme to add space inbetween facet_wrap plots
        axis.ticks = element_blank(),
        axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l =0)),
        axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(family="Times New Roman", colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text=element_text(family="Times New Roman",size=48, color = "Black"), 
        text=element_text(family="Times New Roman",size=48), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))
dev.off()



