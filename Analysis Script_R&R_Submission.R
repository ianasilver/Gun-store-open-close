#GettingStarted ####
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
library(twang)
detectCores()
registerDoParallel(2)

options(scipen=100, digits=12)
options(max.print = 500000000)



setwd("") # Set Working Directory


# Loading CSV (Use Me)####
A<-fread("NSF DATA TRACTS 2020 wInspection.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")

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

names(A)
sink("GunStore Opening_Closing Proportions.txt")
print("2015")
print("0 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==0])
print("1 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==1])
print("2 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==2])
print("3 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==3])
print("4 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==4])
print("5 Gun Stores in 2014 to Gun Stores in 2015")
table(A$FFLs_2015[A$FFLs_2014==5])
print("2016")
print("0 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==0])
print("1 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==1])
print("2 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==2])
print("3 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==3])
print("4 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==4])
print("5 Gun Stores in 2015 to Gun Stores in 2016")
table(A$FFLs_2016[A$FFLs_2015==5])
print("2017")
print("0 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==0])
print("1 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==1])
print("2 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==2])
print("3 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==3])
print("4 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==4])
print("5 Gun Stores in 2016 to Gun Stores in 2017")
table(A$FFLs_2017[A$FFLs_2016==5])
print("2018")
print("0 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==0])
print("1 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==1])
print("2 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==2])
print("3 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==3])
print("4 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==4])
print("5 Gun Stores in 2017 to Gun Stores in 2018")
table(A$FFLs_2018[A$FFLs_2017==5])
print("2019")
print("0 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==0])
print("1 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==1])
print("2 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==2])
print("3 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==3])
print("4 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==4])
print("5 Gun Stores in 2018 to Gun Stores in 2019")
table(A$FFLs_2019[A$FFLs_2018==5])
print("2020")
print("0 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==0])
print("1 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==1])
print("2 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==2])
print("3 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==3])
print("4 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==4])
print("5 Gun Stores in 2019 to Gun Stores in 2020")
table(A$FFLs_2020[A$FFLs_2019==5])
print("2021")
print("0 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==0])
print("1 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==1])
print("2 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==2])
print("3 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==3])
print("4 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==4])
print("5 Gun Stores in 2020 to Gun Stores in 2021")
table(A$FFLs_2021[A$FFLs_2020==5])
print("2022")
print("0 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==0])
print("1 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==1])
print("2 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==2])
print("3 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==3])
print("4 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==4])
print("5 Gun Stores in 2021 to Gun Stores in 2022")
table(A$FFLs_2022[A$FFLs_2021==5])
sink()
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


# Gun store Supplemental Calculations
### 2015 ####
table(A$FFLs_2014, useNA = "always")

A$GunS_Open_2015_Supp<-NA
A$GunS_Open_2015_Supp[A$FFLs_2014 == 0 & A$FFLs_2015 == 1]<-1
table(A$GunS_Open_2015_Supp)
table(A$GunS_Open_2015_Supp,A$FFLs_2014)
A$GunS_Open_2015_Supp[A$FFLs_2014 >=1|A$FFLs_2015 == 0]<-0
table(A$GunS_Open_2015_Supp, useNA = "always")

A$GunS_Closed_2015_Supp<-NA
A$GunS_Closed_2015_Supp[A$FFLs_2014 == 1 & A$FFLs_2015 ==0]<-1
table(A$GunS_Closed_2015_Supp)
table(A$GunS_Closed_2015_Supp,A$FFLs_2014)
A$GunS_Closed_2015_Supp[A$FFLs_2014 == 0|A$FFLs_2015 >=1]<-0
table(A$GunS_Closed_2015_Supp)
table(A$GunS_Closed_2015_Supp,A$FFLs_2014, useNA = "always")

### 2016 ####
table(A$FFLs_2014, useNA = "always")

A$GunS_Open_2016_Supp<-NA
A$GunS_Open_2016_Supp[A$FFLs_2015 == 0 & A$FFLs_2016 == 1]<-1
table(A$GunS_Open_2016_Supp)
table(A$GunS_Open_2016_Supp,A$FFLs_2015)
A$GunS_Open_2016_Supp[A$FFLs_2015 >=1|A$FFLs_2016 == 0]<-0
table(A$GunS_Open_2016_Supp, useNA = "always")

A$GunS_Closed_2016_Supp<-NA
A$GunS_Closed_2016_Supp[A$FFLs_2015 == 1 & A$FFLs_2016 ==0]<-1
table(A$GunS_Closed_2016_Supp)
table(A$GunS_Closed_2016_Supp,A$FFLs_2015)
A$GunS_Closed_2016_Supp[A$FFLs_2015 == 0|A$FFLs_2016 >=1]<-0
table(A$GunS_Closed_2016_Supp)
table(A$GunS_Closed_2016_Supp,A$FFLs_2015, useNA = "always")

### 2017 ####
table(A$FFLs_2016, useNA = "always")

A$GunS_Open_2017_Supp<-NA
A$GunS_Open_2017_Supp[A$FFLs_2016 == 0 & A$FFLs_2017 == 1]<-1
table(A$GunS_Open_2017_Supp)
table(A$GunS_Open_2017_Supp,A$FFLs_2016)
A$GunS_Open_2017_Supp[A$FFLs_2016 >=1|A$FFLs_2017 == 0]<-0
table(A$GunS_Open_2017_Supp, useNA = "always")

A$GunS_Closed_2017_Supp<-NA
A$GunS_Closed_2017_Supp[A$FFLs_2016 == 1 & A$FFLs_2017 ==0]<-1
table(A$GunS_Closed_2017_Supp)
table(A$GunS_Closed_2017_Supp,A$FFLs_2016)
A$GunS_Closed_2017_Supp[A$FFLs_2016 == 0|A$FFLs_2017 >=1]<-0
table(A$GunS_Closed_2017_Supp)
table(A$GunS_Closed_2017_Supp,A$FFLs_2016, useNA = "always")

### 2018 ####
table(A$FFLs_2017, useNA = "always")

A$GunS_Open_2018_Supp<-NA
A$GunS_Open_2018_Supp[A$FFLs_2017 == 0 & A$FFLs_2018 == 1]<-1
table(A$GunS_Open_2018_Supp)
table(A$GunS_Open_2018_Supp,A$FFLs_2017)
A$GunS_Open_2018_Supp[A$FFLs_2017 >=1|A$FFLs_2018 == 0]<-0
table(A$GunS_Open_2018_Supp, useNA = "always")

A$GunS_Closed_2018_Supp<-NA
A$GunS_Closed_2018_Supp[A$FFLs_2017 == 1 & A$FFLs_2018 ==0]<-1
table(A$GunS_Closed_2018_Supp)
table(A$GunS_Closed_2018_Supp,A$FFLs_2017)
A$GunS_Closed_2018_Supp[A$FFLs_2017 == 0|A$FFLs_2018 >=1]<-0
table(A$GunS_Closed_2018_Supp)
table(A$GunS_Closed_2018_Supp,A$FFLs_2017, useNA = "always")

### 2019 ####
table(A$FFLs_2018, useNA = "always")

A$GunS_Open_2019_Supp<-NA
A$GunS_Open_2019_Supp[A$FFLs_2018 == 0 & A$FFLs_2019 == 1]<-1
table(A$GunS_Open_2019_Supp)
table(A$GunS_Open_2019_Supp,A$FFLs_2018)
A$GunS_Open_2019_Supp[A$FFLs_2018 >=1|A$FFLs_2019 == 0]<-0
table(A$GunS_Open_2019_Supp, useNA = "always")

A$GunS_Closed_2019_Supp<-NA
A$GunS_Closed_2019_Supp[A$FFLs_2018 == 1 & A$FFLs_2019 ==0]<-1
table(A$GunS_Closed_2019_Supp)
table(A$GunS_Closed_2019_Supp,A$FFLs_2018)
A$GunS_Closed_2019_Supp[A$FFLs_2018 == 0|A$FFLs_2019 >=1]<-0
table(A$GunS_Closed_2019_Supp)
table(A$GunS_Closed_2019_Supp,A$FFLs_2018, useNA = "always")

### 2020 ####
table(A$FFLs_2019, useNA = "always")

A$GunS_Open_2020_Supp<-NA
A$GunS_Open_2020_Supp[A$FFLs_2019 == 0 & A$FFLs_2020 == 1]<-1
table(A$GunS_Open_2020_Supp)
table(A$GunS_Open_2020_Supp,A$FFLs_2019)
A$GunS_Open_2020_Supp[A$FFLs_2019 >=1|A$FFLs_2020 == 0]<-0
table(A$GunS_Open_2020_Supp, useNA = "always")

A$GunS_Closed_2020_Supp<-NA
A$GunS_Closed_2020_Supp[A$FFLs_2019 == 1 & A$FFLs_2020 ==0]<-1
table(A$GunS_Closed_2020_Supp)
table(A$GunS_Closed_2020_Supp,A$FFLs_2019)
A$GunS_Closed_2020_Supp[A$FFLs_2019 == 0|A$FFLs_2020 >=1]<-0
table(A$GunS_Closed_2020_Supp)
table(A$GunS_Closed_2020_Supp,A$FFLs_2019, useNA = "always")

### 2021 ####
table(A$FFLs_2020, useNA = "always")

A$GunS_Open_2021_Supp<-NA
A$GunS_Open_2021_Supp[A$FFLs_2020 == 0 & A$FFLs_2021 == 1]<-1
table(A$GunS_Open_2021_Supp)
table(A$GunS_Open_2021_Supp,A$FFLs_2020)
A$GunS_Open_2021_Supp[A$FFLs_2020 >=1|A$FFLs_2021 == 0]<-0
table(A$GunS_Open_2021_Supp, useNA = "always")

A$GunS_Closed_2021_Supp<-NA
A$GunS_Closed_2021_Supp[A$FFLs_2020 == 1 & A$FFLs_2021 ==0]<-1
table(A$GunS_Closed_2021_Supp)
table(A$GunS_Closed_2021_Supp,A$FFLs_2020)
A$GunS_Closed_2021_Supp[A$FFLs_2020 == 0|A$FFLs_2021 >=1]<-0
table(A$GunS_Closed_2021_Supp, useNA = "always")
table(A$GunS_Closed_2021_Supp,A$FFLs_2020, useNA = "always")

### 2022 ####
table(A$FFLs_2021, useNA = "always")

A$GunS_Open_2022_Supp<-NA
A$GunS_Open_2022_Supp[A$FFLs_2021 == 0 & A$FFLs_2022 == 1]<-1
table(A$GunS_Open_2022_Supp)
table(A$GunS_Open_2022_Supp,A$FFLs_2021)
A$GunS_Open_2022_Supp[A$FFLs_2021 >=1|A$FFLs_2022 == 0]<-0
table(A$GunS_Open_2022_Supp, useNA = "always")

A$GunS_Closed_2022_Supp<-NA
A$GunS_Closed_2022_Supp[A$FFLs_2021 == 1 & A$FFLs_2022 ==0]<-1
table(A$GunS_Closed_2022_Supp)
table(A$GunS_Closed_2022_Supp,A$FFLs_2021)
A$GunS_Closed_2022_Supp[A$FFLs_2021 == 0|A$FFLs_2022 >=1]<-0
table(A$GunS_Closed_2022_Supp, useNA = "always")
table(A$GunS_Closed_2022_Supp,A$FFLs_2021, useNA = "always")

sink("GunStore Opening_Closing Calculations.txt")
table(A$GunS_Open_2015_Supp, useNA = "always")
table(A$GunS_Closed_2015_Supp, useNA = "always")
table(A$GunS_Open_2016_Supp, useNA = "always")
table(A$GunS_Closed_2016_Supp, useNA = "always")
table(A$GunS_Open_2017_Supp, useNA = "always")
table(A$GunS_Closed_2017_Supp, useNA = "always")
table(A$GunS_Open_2018_Supp, useNA = "always")
table(A$GunS_Closed_2018_Supp, useNA = "always")
table(A$GunS_Open_2019_Supp, useNA = "always")
table(A$GunS_Closed_2019_Supp, useNA = "always")
table(A$GunS_Open_2020_Supp, useNA = "always")
table(A$GunS_Closed_2020_Supp, useNA = "always")
table(A$GunS_Open_2021_Supp, useNA = "always")
table(A$GunS_Closed_2021_Supp, useNA = "always")
table(A$GunS_Open_2022_Supp, useNA = "always")
table(A$GunS_Closed_2022_Supp, useNA = "always")
sink()

# Gun Store Descriptives ####

sink("Gun Store Open_Closing Descriptives.txt")
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



sink("Number of Gun Stores Opening_Closing (More than 2).txt")
print("Opening More than 2: 2015")
table(A$FFLs_2014 == 0 & A$FFLs_2015 >= 2)
print("Opening More than 2: 2016")
table(A$FFLs_2015 == 0 & A$FFLs_2016 >= 2)
print("Opening More than 2: 2017")
table(A$FFLs_2016 == 0 & A$FFLs_2017 >= 2)
print("Opening More than 2: 2018")
table(A$FFLs_2017 == 0 & A$FFLs_2018 >= 2)
print("Opening More than 2: 2019")
table(A$FFLs_2018 == 0 & A$FFLs_2019 >= 2)
print("Opening More than 2: 2020")
table(A$FFLs_2019 == 0 & A$FFLs_2020 >= 2)
print("Opening More than 2: 2021")
table(A$FFLs_2020 == 0 & A$FFLs_2021 >= 2)
print("Opening More than 2: 2022")
table(A$FFLs_2021 == 0 & A$FFLs_2022 >= 2)
print("Closing More than 2: 2015")
table(A$FFLs_2014 >= 2 & A$FFLs_2015 == 0)
print("Closing More than 2: 2016")
table(A$FFLs_2015 >= 2 & A$FFLs_2016 == 0)
print("Closing More than 2: 2017")
table(A$FFLs_2016 >= 2 & A$FFLs_2017 == 0)
print("Closing More than 2: 2018")
table(A$FFLs_2017 >= 2 & A$FFLs_2018 == 0)
print("Closing More than 2: 2019")
table(A$FFLs_2018 >= 2 & A$FFLs_2019 == 0)
print("Closing More than 2: 2020")
table(A$FFLs_2019 >= 2 & A$FFLs_2020 == 0)
print("Closing More than 2: 2021")
table(A$FFLs_2020 >= 2 & A$FFLs_2021 == 0)
print("Closing More than 2: 2022")
table(A$FFLs_2021 >= 2 & A$FFLs_2022 == 0)
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

sink("pre-Imputation Descriptives (Wide).txt")
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

jpeg("Missing Values Plot.jpeg", width = 500, height = 500, quality = 100)
missing_plot(MCDF)
dev.off()

jpeg("Missing Pattern Plot.jpeg", width = 1000, height = 1000, quality = 100)
missing_pattern(MCDF)
dev.off()

sink("Missing Pattern Results.txt")
missing_pattern(MCDF)
sink()


names(MCDF)

sink("Missing Case Analysis (t_test).txt")
stat.desc(MCDF[which(MCDF$CCDI == 1),])
stat.desc(MCDF[which(MCDF$CCDI == 0),])
print("# t-test")
print("# Complete Cases (0 = incomeplete cases; 1 = complete cases)")
table(MCDF$CCDI)
lapply(MCDF[,c(3:122)], function(x, na.rm = FALSE) t.test(x~MCDF$CCDI))
sink()



sink("Missing Case Analysis (Cohen's d).txt")
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
names(G)

# Wide Descriptive Statistics: Post-Imputation ####
names(G)

sink("post-Imputation Descriptives (Wide).txt")
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


# Merging in Spatial Weights #####

A<-fread("Data/WeightsAJPM.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")
names(A)


B<-A[,c("geoid_2020","ContiguityWeight","storesin5mileradius")]

H<-merge(H,B, by = "geoid_2020", all.x = T)
names(H)

# Descriptive Statistics Long ####
sink("post-Imputation Descriptives (Long).txt")
stat.desc(H)
sink()







#______________________________________________####
# R&R                  ####
#______________________________________________####
#@@!!#@$@$#%# Descriptives comparing tracts with and without dealers opening and closing ####


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
table(J$year)

sink("Proportion Open_Closed by year.txt")
stat.desc(J$GunS_Open15_21[J$year==2015])
stat.desc(J$GunS_Open15_21[J$year==2016])
stat.desc(J$GunS_Open15_21[J$year==2017])
stat.desc(J$GunS_Open15_21[J$year==2018])
stat.desc(J$GunS_Open15_21[J$year==2019])
stat.desc(J$GunS_Open15_21[J$year==2020])
stat.desc(J$GunS_Open15_21[J$year==2021])

stat.desc(J$GunS_Closed15_21[J$year==2015])
stat.desc(J$GunS_Closed15_21[J$year==2016])
stat.desc(J$GunS_Closed15_21[J$year==2017])
stat.desc(J$GunS_Closed15_21[J$year==2018])
stat.desc(J$GunS_Closed15_21[J$year==2019])
stat.desc(J$GunS_Closed15_21[J$year==2020])
stat.desc(J$GunS_Closed15_21[J$year==2021])
sink()


names(J)
sink("post-Imputation Descriptives by gunstore opening (15_21).txt")
stat.desc(J[which(J$GunS_Open15_21==1),])
stat.desc(J[which(J$GunS_Open15_21==0),])
lapply(J[,c("total15_22","total16_22","total17_22","total18_22","Year","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","ContiguityWeight")], function(x, na.rm = FALSE) t.test(x~J$GunS_Open15_21))
lapply(J[,c("total15_22","total16_22","total17_22","total18_22","Year","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","ContiguityWeight")], function(x, na.rm = FALSE) cohen.d(x~J$GunS_Open15_21))
sink()

sink("post-Imputation Descriptives by gunstore Closing (15_21).txt")
stat.desc(J[which(J$GunS_Closed15_21==1),])
stat.desc(J[which(J$GunS_Closed15_21==0),])
lapply(J[,c("total15_22","total16_22","total17_22","total18_22","Year","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","ContiguityWeight")], function(x, na.rm = FALSE) t.test(x~J$GunS_Closed15_21))
lapply(J[,c("total15_22","total16_22","total17_22","total18_22","Year","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","ContiguityWeight")], function(x, na.rm = FALSE) cohen.d(x~J$GunS_Closed15_21))
sink()

#@@!!#@$@$#%# Plot Distribution of Dependent Variable: Total Gun Violence ####
names(H)

table(H$total15_22)

jpeg("Plots of Dependent Variable (total15_22).jpeg", width = 1500, height = 1000, quality = 100)
ggplot(H, aes(x = total15_22))+
  geom_histogram(bins = 50)+scale_x_continuous(limits = c(-10,40))+
  coord_cartesian(ylim = c(0,150000),xlim = c(0,40))+
  xlab("Total Gun Violence (15-22)")+
  labs(y = paste0("Frequency"))+
  theme(legend.position= "right",
        legend.justification = 'center',
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.spacing.x = unit(.5, 'cm'),
        legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color="white"),
        legend.box="vertical",
        line = element_line(colour = "black", size = 1), 
        axis.line = element_line(colour = "black"), 
        panel.border= element_blank(), 
        panel.grid.major = element_line(colour = "lightgray"), 
        panel.grid.minor = element_line(colour = "lightgray"), 
        panel.background = element_blank(),
        title = element_text(family="Times New Roman", size=42),
        axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0), size=42),
        axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0), size=42), 
        axis.text= element_text(family="Times New Roman", size=42, color = "Black"), 
        text= element_text(family="Times New Roman", size=42), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")) 
dev.off()


jpeg("Plots of Dependent Variable (total16_22).jpeg", width = 1500, height = 1000, quality = 100)
ggplot(H, aes(x = total16_22))+
  geom_histogram(bins = 50)+scale_x_continuous(limits = c(-10,40))+
  coord_cartesian(ylim = c(0,150000),xlim = c(0,40))+
  xlab("Total Gun Violence (16-22)")+
  labs(y = paste0("Frequency"))+
  theme(legend.position= "right",
        legend.justification = 'center',
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.spacing.x = unit(.5, 'cm'),
        legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color="white"),
        legend.box="vertical",
        line = element_line(colour = "black", size = 1), 
        axis.line = element_line(colour = "black"), 
        panel.border= element_blank(), 
        panel.grid.major = element_line(colour = "lightgray"), 
        panel.grid.minor = element_line(colour = "lightgray"), 
        panel.background = element_blank(),
        title = element_text(family="Times New Roman", size=42),
        axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0), size=42),
        axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0), size=42), 
        axis.text= element_text(family="Times New Roman", size=42, color = "Black"), 
        text= element_text(family="Times New Roman", size=42), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")) 
dev.off()

names(J)

jpeg("Plots of Dependent Variable (total17_22).jpeg", width = 1500, height = 1000, quality = 100)
ggplot(J, aes(x = total17_22))+
  geom_histogram(bins = 50)+scale_x_continuous(limits = c(-10,40))+
  coord_cartesian(ylim = c(0,150000),xlim = c(0,40))+
  xlab("Total Gun Violence (17-22)")+
  labs(y = paste0("Frequency"))+
  theme(legend.position= "right",
        legend.justification = 'center',
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.spacing.x = unit(.5, 'cm'),
        legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color="white"),
        legend.box="vertical",
        line = element_line(colour = "black", size = 1), 
        axis.line = element_line(colour = "black"), 
        panel.border= element_blank(), 
        panel.grid.major = element_line(colour = "lightgray"), 
        panel.grid.minor = element_line(colour = "lightgray"), 
        panel.background = element_blank(),
        title = element_text(family="Times New Roman", size=42),
        axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0), size=42),
        axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0), size=42), 
        axis.text= element_text(family="Times New Roman", size=42, color = "Black"), 
        text= element_text(family="Times New Roman", size=42), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")) 
dev.off()

jpeg("Plots of Dependent Variable (total18_22).jpeg", width = 1500, height = 1000, quality = 100)
ggplot(J, aes(x = total18_22))+
  geom_histogram(bins = 50)+scale_x_continuous(limits = c(-10,40))+
  coord_cartesian(ylim = c(0,150000),xlim = c(0,40))+
  xlab("Total Gun Violence (17-22)")+
  labs(y = paste0("Frequency"))+
  theme(legend.position= "right",
        legend.justification = 'center',
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.spacing.x = unit(.5, 'cm'),
        legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color="white"),
        legend.box="vertical",
        line = element_line(colour = "black", size = 1), 
        axis.line = element_line(colour = "black"), 
        panel.border= element_blank(), 
        panel.grid.major = element_line(colour = "lightgray"), 
        panel.grid.minor = element_line(colour = "lightgray"), 
        panel.background = element_blank(),
        title = element_text(family="Times New Roman", size=42),
        axis.title.x = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0), size=42),
        axis.title.y = element_text(family="Times New Roman",colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0), size=42), 
        axis.text= element_text(family="Times New Roman", size=42, color = "Black"), 
        text= element_text(family="Times New Roman", size=42), 
        plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm")) 
dev.off()

#______________________________________________####
#______________________________________________####
#______________________________________________####
#@@!!#@$@$#%# Replications of Models with Poisson  ####
#______________________________________________####
# Non-Lagged Models                            ####
#______________________________________________####

# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Open15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 15_22 GunStoreOpen 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Closed15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 15_22 GunStoreClosed 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 16_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 16_22 GunStoreClosed 15_21 (controls_No Weights).txt")
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

# Descriptive Statistics Long ####
sink("post-Imputation Descriptives (Long;Subgroup).txt")
stat.desc(J[which(!is.na(J$total16_22)),])
sink()


stat.desc(J[which(!is.na(J$total17_22)),])

# Total Gun Violence 17-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 17_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 17-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 17_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 18-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 18_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 18-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=PO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Poisson_Total 18_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()



#______________________________________________####
#@@!!#@$@$#%# Replications of Models with Normal ####
#______________________________________________####
# Non-Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Open15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 15_22 GunStoreOpen 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Closed15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1



M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 15_22 GunStoreClosed 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]
M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 16_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 16_22 GunStoreClosed 15_21 (controls_No Weights).txt")
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

# Total Gun Violence 17-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 17_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 17-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 17_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 18-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 18_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 18-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Normal_Total 18_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()




#______________________________________________####
#______________________________________________####
#@@!!#@$@$#%# Replications with Count measure of stores ####
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
        "FFLs_2015","FFLs_2016","FFLs_2017","FFLs_2018","FFLs_2019","FFLs_2020","FFLs_2021","FFLs_2022")]

#______________________________________________####
#______________________________________________####
# Imputing Data ####
names(B)
stat.desc(B)


IM_E<-mice(B, m=25, maxit = 1, method = c("rf"), seed = 1992)


IM_F<-complete(IM_E)

stat.desc(IM_F)

fwrite(IM_F,"Data/Imputed Analytical Data (Total Number Gun Stores).csv", buffMB = 1024L, append = F, sep = ",", quote = F, compress = "none")  


# Loading Imputed Data (USE ME) ####
G<-fread("Data/Imputed Analytical Data (Total Number Gun Stores).csv", sep = ",", fill=TRUE, quote=F, na.strings="NA")
names(G)

# Wide Descriptive Statistics: Post-Imputation ####
names(G)

sink("post-Imputation Descriptives (Wide; Total Number Gun Stores).txt")
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

DF3<-reshape(G, varying =c("FFLs_2015","FFLs_2016","FFLs_2017","FFLs_2018","FFLs_2019","FFLs_2020","FFLs_2021","FFLs_2022"), 
             v.names = "FFLs_15_22", timevar = "Year", direction = "long")

names(DF3)
DF3<-(DF3[, c("geoid_2020","FFLs_15_22","Year")])


DF4<-reshape(G, varying =c("FFLs_2015","FFLs_2016","FFLs_2017","FFLs_2018","FFLs_2019","FFLs_2020","FFLs_2021"), 
             v.names = "FFLs_15_21", timevar = "Year", direction = "long")

names(DF4)
DF4<-(DF4[, c("geoid_2020","FFLs_15_21","Year")])

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
          list(long_data,DF1,DF2,DF3,DF4))

names(H)

table(H$FFLs_15_21, useNA = "always")


# Merging in Spatial Weights #####

A<-fread("Data/WeightsAJPM.csv", sep = "\t", fill=TRUE, quote=F, na.strings="NA")
names(A)


B<-A[,c("geoid_2020","ContiguityWeight","storesin5mileradius")]

H<-merge(H,B, by = "geoid_2020", all.x = T)
names(H)


# COVID Variable ####

table(H$year)

H$COVID<-NA
H$COVID[H$year==2020|H$year==2021]<-1
H$COVID[is.na(H$COVID)]<-0
table(H$COVID)

#______________________________________________####
# Models                            ####
#______________________________________________####
# Total Gun Violence 15-22 on FFLs_15_22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","FFLs_15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total15_22~FFLs_15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Total 15_22 FFLs_15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on FFLs_15_21 ####
M1_DF<-H[,c("total16_22","Year","FFLs_15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~FFLs_15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Total 16_22 FFLs_15_21 (controls_No Weights).txt")
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


# Total Gun Violence 17-22 on FFLs_15_21 ####
M1_DF<-J[,c("total17_22","Year","FFLs_15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total17_22~FFLs_15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Total 17_22 FFLs_15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 18-22 on FFLs_15_21 ####
M1_DF<-J[,c("total18_22","Year","FFLs_15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~FFLs_15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Total 18_22 FFLs_15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


#______________________________________________####
#______________________________________________####
#______________________________________________####
#@@!!#@$@$#%# Conduct Randomized Placebo Test ####

H$GunS_Open15_22<-NA
table(H$GunS_Open15_22)
H$GunS_Open15_22<-rbinom(162656,1,.50)
table(H$GunS_Open15_22)

H$GunS_Open15_21<-NA
table(H$GunS_Open15_21)
H$GunS_Open15_21<-rbinom(162656,1,.50)
table(H$GunS_Open15_21)

H$GunS_Closed15_22<-NA
table(H$GunS_Closed15_22)
H$GunS_Closed15_22<-rbinom(162656,1,.50)
table(H$GunS_Closed15_22)

H$GunS_Closed15_21<-NA
table(H$GunS_Closed15_21)
H$GunS_Closed15_21<-rbinom(162656,1,.50)
table(H$GunS_Closed15_21)
#______________________________________________####
#______________________________________________####
# Total Gun Violence 15-22 on Gun Stores Open 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Open15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total15_22~GunS_Open15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 15_22 GunStoreOpen 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 15-22 on Gun Stores Closed 15-22 ####

names(H)

M1_DF<-H[,c("total15_22","Year","GunS_Closed15_22","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

stat.desc(M1_DF)

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total15_22~GunS_Closed15_22+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
            family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 15_22 GunStoreClosed 15_22 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

#______________________________________________####
# Lagged Models                            ####
#______________________________________________####
# Total Gun Violence 16-22 on Gun Stores Open 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 16_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()


# Total Gun Violence 16-22 on Gun Stores Closed 15-21 ####

M1_DF<-H[,c("total16_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total16_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 16_22 GunStoreClosed 15_21 (controls_No Weights).txt")
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

# Total Gun Violence 17-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 17_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 17-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total17_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]

M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1


M1<-gamlss(total17_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 17_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()



# Total Gun Violence 18-22 on Gun Stores Open 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Open15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Open15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 18_22 GunStoreOpen 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()

# Total Gun Violence 18-22 on Gun Stores Closed 15-21 ####

M1_DF<-J[,c("total18_22","Year","GunS_Closed15_21","prowhite",                
            "problack","prohispanic","f_m_sexratio",            
            "pctYoungMale1524","pctLivingAlone","PublicAssist",            
            "totalpop",     
            "RentBurden","CDIS","geoid_2020","ContiguityWeight")]


M1_DF<-M1_DF[complete.cases(M1_DF),]


M1_DF$ContiguityWeight<-M1_DF$ContiguityWeight+.1

M1<-gamlss(total18_22~GunS_Closed15_21+Year+
             prowhite+problack+prohispanic+f_m_sexratio+pctYoungMale1524+
             pctLivingAlone+PublicAssist+totalpop+
             RentBurden+CDIS+re(random=~1|as.factor(geoid_2020)),
           robust = T, data=M1_DF, weights = M1_DF$ContiguityWeight, 
           family=NO, control = gamlss.control(c.crit = .01))

SM1<-data.frame(summary(M1))
UCI_M1<-SM1$Estimate+1.96*SM1$Std..Error
LCI_M1<-SM1$Estimate-1.96*SM1$Std..Error




sink("Random_Total 18_22 GunStoreClosed 15_21 (controls_No Weights).txt")
summary(M1)
cbind("b" = SM1$Estimate,"SE" = SM1$Std..Error,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1)
exp(cbind("b" = SM1$Estimate,"Lower 95% CI" = LCI_M1, "Upper 95% CI" = UCI_M1))
summary(residuals(M1))
sink()




#______________________________________________####
# Figure (Poisson)                                      ####
#______________________________________________####

DV<-c("Total Shootings (16-22) - One Year Lag","Total Shootings (16-22) - One Year Lag",
      "Total Shootings (17-22) - Two Year Lag","Total Shootings (17-22) - Two Year Lag",
      "Total Shootings (18-22) - Three Year Lag","Total Shootings (18-22) - Three Year Lag") # Heading for Facet Wrap
IV<-c("Firearm Dealer Openings","Firearm Dealer Closings") # Independent variable names
ES<-c(0.0269841932344197,0.028875430998478,
      0.04435654944130,-0.05639780269002120,
      0.139827042272579666,0.021211081726652811) # b Estimate (could be standardized estimate, Odds Ratio, Incident Rate Ratio, etc.)
LCI<-c(-0.0151261565103212,-0.0133812430669116,
       -0.01636192310544864,-0.1029116895084948,
       0.07155670757841080,-0.05847690197930018) # Lower 95% confidence interval
UCI<-c(0.0690945429791605,0.07113210506387,
       0.105075021988056461,-0.009883915871547563,
       0.20809737696674852,0.10089906543260581) # Upper 95% confidence interval

A<-data.frame(DV,IV,ES,LCI,UCI)

A$IV<-factor(A$IV, levels=c("Firearm Dealer Closings","Firearm Dealer Openings"))

A$DV<-factor(A$DV, levels=c("Total Shootings (16-22) - One Year Lag",
                            "Total Shootings (17-22) - Two Year Lag",
                            "Total Shootings (18-22) - Three Year Lag"))

jpeg("slope coefficients for Poisson Models.jpeg", width = 3000, height = 3000, quality = 100)
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
        strip.text.x = element_text(family="Times New Roman",size=70, color = "Black"),
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
        axis.text=element_text(family="Times New Roman",size=70, color = "Black"), 
        text=element_text(family="Times New Roman",size=70), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))
dev.off()


#______________________________________________####
# Figure (Normal)                                      ####
#______________________________________________####

DV<-c("Total Shootings (16-22) - One Year Lag","Total Shootings (16-22) - One Year Lag",
      "Total Shootings (17-22) - Two Year Lag","Total Shootings (17-22) - Two Year Lag",
      "Total Shootings (18-22) - Three Year Lag","Total Shootings (18-22) - Three Year Lag") # Heading for Facet Wrap
IV<-c("Firearm Dealer Openings","Firearm Dealer Closings") # Independent variable names
ES<-c(0.0083247028934977,0.04955644144644113,
      0.07544776919925436,-0.04698209349350,
      0.13342124997551383,0.01523699214151395) # b Estimate (could be standardized estimate, Odds Ratio, Incident Rate Ratio, etc.)
LCI<-c(-0.04472327654664116,-0.0072868158299511,
       0.0074180298121165611,-0.105926430738662911,
       0.05640172165402307,-0.07920883427514239) # Lower 95% confidence interval
UCI<-c(0.06137268233363669,0.10639969872283338,
       0.143477508586392,0.011962243751659,
       0.21044077829700458,0.10968281855817028) # Upper 95% confidence interval

A<-data.frame(DV,IV,ES,LCI,UCI)

A$IV<-factor(A$IV, levels=c("Firearm Dealer Closings","Firearm Dealer Openings"))

A$DV<-factor(A$DV, levels=c("Total Shootings (16-22) - One Year Lag",
                            "Total Shootings (17-22) - Two Year Lag",
                            "Total Shootings (18-22) - Three Year Lag"))

jpeg("slope coefficients for Normal Models.jpeg", width = 3000, height = 3000, quality = 100)
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
        strip.text.x = element_text(family="Times New Roman",size=70, color = "Black"),
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
        axis.text=element_text(family="Times New Roman",size=70, color = "Black"), 
        text=element_text(family="Times New Roman",size=70), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))
dev.off()
