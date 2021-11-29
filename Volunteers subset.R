
library(dplyr)
library(tidyr)
library(gmodels)
library(boot)
library(ggm)
library(Hmisc)
library(polycor)
library(QuantPsyc)
library(car)
library(foreign)


share <- read.dta('data/easySHARE_rel7-0-0.dta')
share_w2<-share[which(share$wave==2),]
#37152 (vs30424 wave 1) obs.

# - ac002d1: voluntary or charity work  
class(share_w2$ac002d1)
#[1] "factor"
table(share_w2$ac002d1)
#ricodificare variabile
share_w2$charity<-as.numeric(share_w2$ac002d1)
table(share_w2$charity)
share_w2$charity[which(share_w2$charity<11)]<-NA
share_w2$charity[which(share_w2$charity==11)]<-0
share_w2$charity[which(share_w2$charity==12)]<-1
table(share_w2$charity)
# 0. Not selected                           1. Selected
#         31877                                  4410


volunteers<-share_w2[which(share_w2$charity==1),]
volunteers



# AGE AT INTERVIEW (age)
class(volunteers$age)
#[1] "numeric"
table(volunteers$age)
#1 osservazione con -15
mean(volunteers$age, na.rm = T)
#[1] 63.62 (vs64.26378 wave 1)
#ricodificare variabile
volunteers$age[which(share_w2$age<0)]<- NA


# QUALITY OF LIFE (casp)
class(volunteers$casp)
#[1] "integer"
table(volunteers$casp)
# -15
# 2752
#ricodificare variabile
volunteers$casp[which(volunteers$casp<0)]<-NA
table(volunteers$casp)
mean(volunteers$casp, na.rm = T)
#[1] 36.95878



# - scarsa (poor): 495 obs.
volunteers$scarsa= 0
volunteers$scarsa[volunteers$casp<=21]= 1
table(volunteers$scarsa)

# - discreta (fair): 6439 obs.
volunteers$discreta= 0
volunteers$discreta[volunteers$casp>=22 & volunteers$casp<=31]= 1
table(volunteers$discreta)

# - buona (good): 18257 obs.
volunteers$buona= 0
volunteers$buona[volunteers$casp>=32 & volunteers$casp<=41]= 1
table(volunteers$buona)

# - molto buona (very good): 11308 obs.
volunteers$molto_buona= 0
volunteers$molto_buona[volunteers$casp>=41]= 1
table(volunteers$molto_buona)

# - creo cvariabile ordinale per CASP
volunteers$healthCASP = 0
volunteers$healthCASP[volunteers$casp<=22]= 1
volunteers$healthCASP[volunteers$casp>=23 & volunteers$casp<=31]=2
volunteers$healthCASP[volunteers$casp>=32 & volunteers$casp<=41]=3
volunteers$healthCASP[volunteers$casp>=41]=4
volunteers$healthCASP[which(volunteers$healthCASP == 0)]<- NA
table(volunteers$healthCASP)


# GENDER OF RESPONDENT (female)
class(volunteers$female)
#[1] "factor"
table(volunteers$female)
#ricodificare variabile
volunteers$gender<- as.numeric(volunteers$female)
volunteers$gender[which(volunteers$gender<0)]<-NA
table(volunteers$gender)
#   male  female 
# 16404   20748 

CrossTable(volunteers$gender, volunteers$healthCASP, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
