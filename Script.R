mypath <- ("C:/Users/Luigi/Downloads/Ricerca Applicata e Disuguaglianze")
setwd(mypath)
getwd()

install.packages("dplyr") 
install.packages("tidyr")
install.packages("gmodels")
install.packages("Hmisc")
install.packages("ggm")
install.packages("polycor")
install.packages("QuantPsyc")
install.packages("car")
install.packages("foreign")


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

#1 - ANALIZZARE VARIABILI DI INTERESSE

# AGE AT INTERVIEW (age)
class(share_w2$age)
#[1] "numeric"
table(share_w2$age)
#1 osservazione con -15
mean(share_w2$age, na.rm = T)
#[1] 65.07701 (vs64.26378 wave 1)
#ricodificare variabile
share_w2$age[which(share_w2$age<0)]<- NA



# GENDER OF RESPONDENT (female)
class(share_w2$female)
#[1] "factor"
table(share_w2$female)
#ricodificare variabile
share_w2$gender<- as.numeric(share_w2$female)
share_w2$gender[which(share_w2$gender<0)]<-NA
table(share_w2$gender)
#   male  female 
# 16404   20748 



#MODIFIED COUNTRY IDENTIFIER (country_mod)
class(share_w2$country_mod)
#[1] "factor"
unique(share_w2$country_mod)
table(share_w2$country_mod)



# AREA OF LOCATION (iv009_mod)
class(share_w2$iv009_mod)
#[1] "factor"
table(share_w2$iv009_mod)
#ricodificare variabile
share_w2$loc<-as.numeric(share_w2$iv009_mod)
unique(share_w2$loc)
table(share_w2$loc)
# valori 1 (-15) , 4 (-12), 7 (-9) vanno messi a missing (NA)
#ricodificare da 1 a 5

share_w2$loc[which(share_w2$loc<12)]<- NA
share_w2$loc[which(share_w2$loc==12)] <-1
share_w2$loc[which(share_w2$loc==13)] <-2
share_w2$loc[which(share_w2$loc==14)] <-3
share_w2$loc[which(share_w2$loc==15)] <-4
share_w2$loc[which(share_w2$loc==16)] <-5
table(share_w2$loc)



# ISCED CLASSIFICATION (isced1997_r)
class(share_w2$isced1997_r)
#[1] "factor"
table(share_w2$isced1997_r)
#ricodificare variabile
share_w2$edu<-as.numeric(share_w2$isced1997_r)
share_w2$edu[which(share_w2$edu<0)]<-NA
table(share_w2$edu)
#    1     4    11    12    13    14    15    16    17    18    19 
# 475    25  1652  9744  6210 10407  1369  6893   139    21   217
#dopo la ricodifica l'etichetta "no information" diventa un numero (1)
share_w2$edu[which(share_w2$edu<11 | share_w2$edu>=18)]<- NA
share_w2$edu[which(share_w2$edu==11)] <-0
share_w2$edu[which(share_w2$edu==12)] <-1
share_w2$edu[which(share_w2$edu==13)] <-2
share_w2$edu[which(share_w2$edu==14)] <-3
share_w2$edu[which(share_w2$edu==15)] <-4
share_w2$edu[which(share_w2$edu==16)] <-5
share_w2$edu[which(share_w2$edu==17)] <-6

table(share_w2$edu)
#    0     1     2     3     4     5     6 
#1652  9744  6210 10407  1369  6893   139 



# SELF-PERCEIVED HEALTH (sphus)
class(share_w2$sphus)
#[1] "factor"
table(share_w2$sphus)
#ricodificare variabile
share_w2$health<-as.numeric(share_w2$sphus)
table(share_w2$health)
#    1     4    11    12    13    14    15 
# 100    33  3338  6966 13412  9232  4071
# 1 and 4 --> missing values

share_w2$health[which(share_w2$health<11)]<-NA
share_w2$health[which(share_w2$health==11)]<-1
share_w2$health[which(share_w2$health==12)]<-2
share_w2$health[which(share_w2$health==13)]<-3
share_w2$health[which(share_w2$health==14)]<-4
share_w2$health[which(share_w2$health==15)]<-5

table(share_w2$health)
#   1     2     3     4     5 
#3338  6966 13412  9232  4071 



# ACTIVITIES (from ac002d1 to ac002dno)

#Which activities affect more the health?

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

# - ac002d2: Cared for a sick or disabled adult
table(share_w2$ac002d2)
share_w2$care<-as.numeric(share_w2$ac002d2)
table(share_w2$care)
share_w2$care[which(share_w2$care<11)]<-NA
share_w2$care[which(share_w2$care==11)]<-0
share_w2$care[which(share_w2$care==12)]<-1
table(share_w2$care)
# 0. Not selected                           1. Selected
#         33581                                  2706

# - ac002d3: Provided help to family, friends or neighbors
table(share_w2$ac002d3)
share_w2$providehelp<-as.numeric(share_w2$ac002d3)
table(share_w2$providehelp)
share_w2$providehelp[which(share_w2$providehelp<11)]<-NA
share_w2$providehelp[which(share_w2$providehelp==11)]<-0
share_w2$providehelp[which(share_w2$providehelp==12)]<-1
table(share_w2$providehelp)
# 0. Not selected                           1. Selected
#         30149                                  6138

# - ac002d4: Attended an educational or training course
table(share_w2$ac002d4)
#ricodificare variabile
share_w2$course<-as.numeric(share_w2$ac002d4)
table(share_w2$course)
share_w2$course[which(share_w2$course<11)]<-NA
share_w2$course[which(share_w2$course==11)]<-0
share_w2$course[which(share_w2$course==12)]<-1
table(share_w2$course)
# 0. Not selected                           1. Selected
#         33598                                  2689

#ac002d5: Gone to a sport, social or other kind of club
table(share_w2$ac002d5)
#ricodificare variabile
share_w2$club<-as.numeric(share_w2$ac002d5)
table(share_w2$club)
share_w2$club[which(share_w2$club<11)]<-NA
share_w2$club[which(share_w2$club==11)]<-0
share_w2$club[which(share_w2$club==12)]<-1
table(share_w2$club)
# 0. Not selected                           1. Selected
#         29163                                  7124

#ac002d6: Taken part in a religious organization
table(share_w2$ac002d6)
#ricodificare variabile
share_w2$church<-as.numeric(share_w2$ac002d6)
table(share_w2$church)
share_w2$church[which(share_w2$church<11)]<-NA
share_w2$church[which(share_w2$church==11)]<-0
share_w2$church[which(share_w2$church==12)]<-1
table(share_w2$church)
# 0. Not selected                           1. Selected
#         31980                                  4307

#ac002d7: Taken part in a political or community-related organization
table(share_w2$ac002d7)
share_w2$political<-as.numeric(share_w2$ac002d7)
table(share_w2$political)
share_w2$political[which(share_w2$political<11)]<-NA
share_w2$political[which(share_w2$political==11)]<-0
share_w2$political[which(share_w2$political==12)]<-1
table(share_w2$political)
# 0. Not selected                           1. Selected
#         34776                                  1511 

#ac002dno: None of these
table(share_w2$ac002dno)
# 0. Not selected                           1. Selected
#        17710                                 18978



# QUALITY OF LIFE (casp)
class(share_w2$casp)
#[1] "integer"
table(share_w2$casp)
# -15
# 2752
#ricodificare variabile
share_w2$casp[which(share_w2$casp<0)]<-NA
table(share_w2$casp)
mean(share_w2$casp, na.rm = T)
#[1] 36.95878


# HOUSEHOLD INCOME PERCENTILES (income_pct_w2)
class(share_w2$income_pct_w2)
#[1] "integer"
table(share_w2$income_pct_w2)
#ricodificare variabile
share_w2$income_pct_w2[which(share_w2$income_pct_w2<0)]<-NA
table(share_w2$income_pct_w2)



#DEPRESSION SCALE EURO-D (eurod)
class(share_w2$eurod)
#[1] "integer"
table(share_w2$eurod)
#ricodificare variabile
share_w2$eurod[which(share_w2$eurod<0)]<-NA
table(share_w2$eurod)
#-15    0    1    2    3    4    5    6    7    8    9   10   11   12 
#1169 8847 7968 5849 4333 3070 2138 1446  966  664  372  224   88   18 


# 2. ESPLORARE RELAZIONI: chi2, residui standardizzati, odds ratio

country<-share_w2 %>% group_by(country_mod) %>% summarise(average = mean(casp,na.rm = T))
country

# A tibble: 15 x 2
#country_mod      average
#<fct>              <dbl>
#1 40. Austria         37.8
#2 56. Belgium         37.3
#3 203. Czechia        35.0
#4 208. Denmark        40.6
#5 250. France         36.7
#6 276. Germany        38.6
#7 300. Greece         34.1
#8 372. Ireland        39.1
#9 376. Israel         35.9
#10 380. Italy          33.4
#11 528. Netherlands    40.4
#12 616. Poland         34.5
#13 724. Spain          35.4
#14 752. Sweden         38.9
#15 756. Switzerland    40.5


#DIVIDERE LA CASP (QUALITY OF LIFE) IN QUATTRO CATEGORIE: scarsa, discreta, buona, molto buona
#Range 12 a 48
# - scarsa (poor): 12-21
# - discreta (fair): 22-31
# - buona (good): 32-41
# - molto buona (very good): 42-48

table(share_w2$casp)

#creare dummy per variabile CASAP
mean(share_w2$casp, na.rm = T)
#[1] 36.95878

# - scarsa (poor): 495 obs.
share_w2$scarsa= 0
share_w2$scarsa[share_w2$casp<=21]= 1
table(share_w2$scarsa)

# - discreta (fair): 6439 obs.
share_w2$discreta= 0
share_w2$discreta[share_w2$casp>=22 & share_w2$casp<=31]= 1
table(share_w2$discreta)

# - buona (good): 18257 obs.
share_w2$buona= 0
share_w2$buona[share_w2$casp>=32 & share_w2$casp<=41]= 1
table(share_w2$buona)

# - molto buona (very good): 11308 obs.
share_w2$molto_buona= 0
share_w2$molto_buona[share_w2$casp>=41]= 1
table(share_w2$molto_buona)

# - creo cvariabile ordinale per CASP
share_w2$healthCASP = 0
share_w2$healthCASP[share_w2$casp<=22]= 1
share_w2$healthCASP[share_w2$casp>=23 & share_w2$casp<=31]=2
share_w2$healthCASP[share_w2$casp>=32 & share_w2$casp<=41]=3
share_w2$healthCASP[share_w2$casp>=41]=4
share_w2$healthCASP[which(share_w2$healthCASP == 0)]<- NA
table(share_w2$healthCASP)

#distribuzione della variabile CASP (poor) e genere
table(share_w2$gender)
table(share_w2$scarsa, share_w2$gender)

#9(men)    10(women)
#0 16245 20412
#1   159   336

#creo variabile "activities" con tuttte le attività considerate
share_w2$activities = 0
share_w2$activities[which(share_w2$charity==1)]=1
share_w2$activities[which(share_w2$church==1)]=1
share_w2$activities[which(share_w2$club==1)]=1
share_w2$activities[which(share_w2$care==1)]=1
share_w2$activities[which(share_w2$providehelp==1)]=1
share_w2$activities[which(share_w2$course==1)]=1
share_w2$activities[which(share_w2$political==1)]=1
table(share_w2$activities)

table(share_w2$charity)
table(share_w2$healthCASP)
CrossTable(share_w2$activities, share_w2$healthCASP, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")

#TABELLA DI CONTINGENZA
CrossTable(share_w2$gender, share_w2$healthCASP, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")

#Pearson's Chi-squared test 
#------------------------------------------------------------
#Chi^2 =  29.45663     d.f. =  1     p =  5.718198e-08 
#The chi-square value of 29.45, with 1 degree of freedom, is highly significant because the p-value is less than .05
#residui standardizzati ci dicono che esiste relazione tra genere (GENDER) e qualità della vita (CASP)
#(freq. osservate - freq. attese)/???freq. attese
#       (159     -     218) / ???218 ---> -3.995
#       (336    -     278)  / ???278 ---> 3.611
#CALCOLO: (16245/159)/(20412/336)
#ODDS UOMO = 102 --> per ogni persona di sesso maschile con una scarsa qualità di vita ce ne sono 102 che hanno una qualità di vita differente
#ODDS DONNA = 61 --> per ogni persona di sesso femminile con una scarsa qualità di vita ce ne sono 60 che hanno una qualità di vita differente
#ODDS RATIO = 1.68 --> è 1.68 volte meno frequente tra gli uomini avere una qualità di vita scarsa (rispetto ad averla) che tra le donne


CrossTable(share_w2$gender, share_w2$healthCASP, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")


table(share_w2$charity)
table(share_w2$volunteer)
CrossTable(share_w2$charity, share_w2$healthCASP, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")


#tabelle di contingenza per "voluntary or charity work" e "quality of life"
CrossTable(share_w2$charity, share_w2$scarsa, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$charity, share_w2$discreta, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$charity, share_w2$buona, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$charity, share_w2$molto_buona, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")


#Cell Contents
#   |-------------------------|
#  |                   Count |
#  |         Expected Values |
#  |           Total Percent |
#  |            Std Residual |
#  |-------------------------|
#  
#  Total Observations in Table:  36287 
#
#                   | share_w2$molto_buona 
#share_w2$charity   |        0  |        1  | Row Total | 
#  -----------------|-----------|-----------|-----------|
#  0                |    22777  |     9100  |    31877  | 
#                   | 21982.805  | 9894.195  |           | 
#                    |   62.769% |   25.078% |           | 
#                   |    5.357  |   -7.984  |           | 
#  -----------------|-----------|-----------|-----------|
#  1                |     2247  |     2163  |     4410  | 
#                   | 3041.195  | 1368.805  |           | 
#                   |    6.192% |    5.961% |           | 
#                   |  -14.401  |   21.466  |           | 
#  -----------------|-----------|-----------|-----------|
#  Column Total |    25024  |    11263  |    36287  | 
#  -----------------|-----------|-----------|-----------|
#  
  
#  Statistics for All Table Factors


#Pearson's Chi-squared test 
#------------------------------------------------------------
#Chi^2 =  760.6424     d.f. =  1     p =  1.947032e-167 

#Pearson's Chi-squared test with Yates' continuity correction 
#------------------------------------------------------------
#Chi^2 =  759.685     d.f. =  1     p =  3.144515e-167 

 
#       Minimum expected frequency: 1368.805
#TUTTI I RESIDUI STANDARDIZZATI INDICANO UNA RELAZIONE TRA LE DUE VARIABILI



#tabelle di contingenza per "Taken part in a religious organization"
CrossTable(share_w2$church, share_w2$scarsa, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$church, share_w2$discreta, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$church, share_w2$buona, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(share_w2$church, share_w2$molto_buona, chisq = TRUE, expected = TRUE, sresid = TRUE, prop.r=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")



#dato descrittivo
volunteer<-share_w2 %>% group_by(country_mod, gender, share_w2$volunteer) %>% summarise(vol_rate = mean(casp,na.rm = T))
volunteer
# A tibble: 90 x 4
# Groups:   country_mod, gender [30]
#country_mod gender `share_w2$charity` casp_average
#<fct>        <dbl>              <dbl>        <dbl>
#1 40. Austria      9                  0         38.0
#2 40. Austria      9                  1         40.2
#3 40. Austria      9                 NA         36.5
#4 40. Austria     10                  0         37.3
#5 40. Austria     10                  1         39.9
#6 40. Austria     10                 NA         36.3
View(volunteer)
vol<-as_tibble(volunteer)
head(vol)
tail(vol)


#a prescindere dal livello della qualità di vita e dal genere, chi fa volontariato ha una CASP in media più alta

religious<-share_w2 %>% group_by(country_mod, gender, share_w2$church) %>% summarise(casp_average = mean(casp,na.rm = T))
religious
# A tibble: 90 x 4
# Groups:   country_mod, gender [30]
#country_mod gender `share_w2$church` casp_average
#<fct>        <dbl>             <dbl>        <dbl>
#1 40. Austria      9                 0         38.0
#2 40. Austria      9                 1         41.2
#3 40. Austria      9                NA         36.5
#4 40. Austria     10                 0         37.3
#5 40. Austria     10                 1         39.7
#6 40. Austria     10                NA         36.3

club<-share_w2 %>% group_by(country_mod, gender, share_w2$club) %>% summarise(casp_average = mean(casp,na.rm = T))
club

#dicotomizzare seguenti variabili:

# - AGE AT INTERVIEW (age) --> dividerla in categorie
#ci sono 1067 casi al di sotto dei 50 che andranno persi con la centratura

#correlation
cor(share_w2$age, share_w2$casp, use = "complete.obs", method = "pearson")
#[1] -0.1905074

#età centrata
share_w2$age[which(share_w2$age<50)]<- NA
table(share_w2$age)
share_w2$age_cen= share_w2$age - 50
#invece di 50 anni si può usare l'età media (65) 
table(share_w2$age_cen)

#età al quadrato
share_w2$age2 = share_w2$age_cen*share_w2$age_cen/100
#se age cen è positiva e age2 no significa che cambia la tendenza: non è lineare

share_w2$age_fifty=0
share_w2$age_fifty[which(share_w2$age_cen<10)]=1
table(share_w2$age_fifty)
#0     1 
#24535 12617 

share_w2$age_sixty=0
share_w2$age_sixty[which(share_w2$age_cen>=10 & share_w2$age_cen<20)]=1
table(share_w2$age_sixty)
#0     1 
#25404 11748

share_w2$age_seventy=0
share_w2$age_seventy[which(share_w2$age_cen>=20 & share_w2$age_cen<30)]=1
table(share_w2$age_seventy)
# 0     1 
#29081  8071

share_w2$age_eighty=0
share_w2$age_eighty[which(share_w2$age_cen>=30)]=1
table(share_w2$age_eighty)
# 0     1 
#33504  3648 



# - AREA OF LOCATION (iv009_mod)
table(share_w2$iv009_mod)

share_w2$big_city= 0
share_w2$big_city[which(share_w2$loc==1)]=1
table(share_w2$big_city)

share_w2$outskirts= 0
share_w2$outskirts[which(share_w2$loc==2)]=1

share_w2$large_town= 0
share_w2$large_town[which(share_w2$loc==3)]=1

share_w2$small_town= 0
share_w2$small_town[which(share_w2$loc==4)]=1

share_w2$village= 0
share_w2$village[which(share_w2$loc==5)]=1
table(share_w2$village)



# - ISCED CLASSIFICATION (isced1997_r)
table(share_w2$isced1997_r)
table(share_w2$edu)

share_w2$low_level=0
share_w2$low_level[which(share_w2$edu==0)]=1
share_w2$low_level[which(share_w2$edu==1)]=1
share_w2$low_level[which(share_w2$edu==2)]=1
table(share_w2$low_level)
#0     1 
#19546 17606 

share_w2$moderate=0
share_w2$moderate[which(share_w2$edu==3)]=1
share_w2$moderate[which(share_w2$edu==4)]=1
table(share_w2$moderate)
#0     1 
#25376 11776 

share_w2$high_level=0
share_w2$high_level[which(share_w2$edu==5)]=1
share_w2$high_level[which(share_w2$edu==6)]=1
table(share_w2$high_level)
#0     1 
#30120  7032 



# - SELF-PERCEIVED HEALTH (sphus)
table(share_w2$sphus)
table(share_w2$health)

share_w2$poor=0
share_w2$poor[which(share_w2$health==5)]=1
table(share_w2$poor)

share_w2$fair=0
share_w2$fair[which(share_w2$health==4)]=1

share_w2$good=0
share_w2$good[which(share_w2$health==3)]=1

share_w2$very_good=0
share_w2$very_good[which(share_w2$health==2)]=1

share_w2$excellent=0
share_w2$excellent[which(share_w2$health==1)]=1



# - MODIFIED COUNTRY IDENTIFIED (country_mod)
table(share_w2$country_mod)

table(share_w2$country_mod=="380. Italy", share_w2$charity)
table(share_w2$country_mod=="380. Italy", share_w2$church)
share_w2$Italy=0
share_w2$Italy[which(share_w2$country_mod=="380. Italy")]=1


table(share_w2$country_mod=="724. Spain", share_w2$charity)
table(share_w2$country_mod=="724. Spain", share_w2$church)
share_w2$Spain=0
share_w2$Spain[which(share_w2$country_mod=="724. Spain")]=1


table(share_w2$country_mod=="300. Greece", share_w2$charity)
table(share_w2$country_mod=="300. Greece", share_w2$church)
share_w2$Greece=0
share_w2$Greece[which(share_w2$country_mod=="300. Greece")]=1

table(share_w2$country_mod=="752. Sweden", share_w2$charity)
table(share_w2$country_mod=="752. Sweden", share_w2$church)
share_w2$Sweden=0
share_w2$Sweden[which(share_w2$country_mod=="752. Sweden")]=1


table(share_w2$country_mod=="528. Netherlands", share_w2$charity)
table(share_w2$country_mod=="528. Netherlands", share_w2$church)
share_w2$Netherlands=0
share_w2$Netherlands[which(share_w2$country_mod=="528. Netherlands")]=1


table(share_w2$country_mod=="208. Denmark", share_w2$charity)
table(share_w2$country_mod=="208. Denmark", share_w2$church)
share_w2$Denmark=0
share_w2$Denmark[which(share_w2$country_mod=="208. Denmark")]=1
table(share_w2$Denmark)



# - ACTIVITIES
share_w2$volunteer=0
share_w2$volunteer[which(share_w2$charity==1)]=1
table(share_w2$charity)
table(share_w2$volunteer)


share_w2$religious=0
share_w2$religious[which(share_w2$church==1)]=1
table(share_w2$church)
table(share_w2$religious)

share_w2$sport=0
share_w2$sport[which(share_w2$club==1)]=1
table(share_w2$club)
table(share_w2$sport)

share_w2$provide_care=0
share_w2$provide_care[which(share_w2$care==1)]=1
table(share_w2$care)
table(share_w2$provide_care)



# - #DEPRESSION SCALE EURO-D (eurod)

share_w2$presence_of_feeling= 0
share_w2$presence_of_feeling[which(share_w2$eurod==1)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==2)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==3)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==4)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==5)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==6)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==7)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==8)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==9)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==10)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==11)]=1
share_w2$presence_of_feeling[which(share_w2$eurod==12)]=1
table(share_w2$presence_of_feeling)



# 3. REGRESSIONE LINEARE E INTERAZIONE

#Si prende come riferimento un uomo di 50 anni, che si percepisce in buona salute,
#non svolge volontariato o sport e non prende parte a funzioni e/o gruppi religiosi,
#ha un livello educativo medio, vive in un paese Mediterraneo e abita in una metropoli
regression_1<-lm(casp ~ age_sixty + age_seventy + age_eighty + gender + poor + fair + very_good + excellent + volunteer + religious + sport + Netherlands + Denmark + Sweden + low_level + high_level + outskirts + large_town + small_town + village, data = share_w2)
summary(regression_1)
# REGRESSIONE lineare semplice: funzione lm()
#oggetto <- lm(DV ~ IV + IV + IV, data = dataset)

#  lm(formula = casp ~ age_sixty + age_seventy + age_eighty + 
#       gender + poor + fair + very_good + excellent + volunteer + 
#       religious + sport + Netherlands + Denmark + Sweden + low_level + 
#       high_level + outskirts + large_town + small_town + village, 
#     data = share_w2)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 41.16612    0.55666  73.952  < 2e-16 ***
#  age_sixty    0.38739    0.06859   5.648 1.63e-08 ***
#  age_seventy -0.07803    0.07936  -0.983  0.32551    
# age_eighty  -1.37499    0.10961 -12.545  < 2e-16 ***
#  gender      -0.41718    0.05726  -7.285 3.28e-13 ***
#  poor        -6.88766    0.10348 -66.560  < 2e-16 ***
#  fair        -2.56211    0.07429 -34.489  < 2e-16 ***
#  very_good    1.38277    0.08015  17.253  < 2e-16 ***
#  excellent    2.61838    0.10580  24.749  < 2e-16 ***
#  volunteer    0.96335    0.09072  10.619  < 2e-16 ***
#  religious    0.04065    0.08885   0.458  0.64731    
#   sport        1.08605    0.07452  14.575  < 2e-16 ***
#  Netherlands  3.28277    0.11215  29.272  < 2e-16 ***
#  Denmark      2.45817    0.11362  21.635  < 2e-16 ***
#  Sweden       1.77393    0.10914  16.253  < 2e-16 ***
#  low_level   -1.51223    0.06616 -22.858  < 2e-16 ***
#  high_level   0.17483    0.08168   2.140  0.03233 *  
#  outskirts    0.40708    0.09851   4.132 3.60e-05 ***
#  large_town   0.25433    0.09091   2.798  0.00515 ** 
#  small_town   0.77438    0.08826   8.774  < 2e-16 ***
#  village      0.38273    0.08337   4.591 4.43e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 5.225 on 34379 degrees of freedom
#(2752 observations deleted due to missingness)
#Multiple R-squared:  0.3126,	Adjusted R-squared:  0.3122 
#F-statistic: 781.6 on 20 and 34379 DF,  p-value: < 2.2e-16

confint(regression_1)

#aggiungere altri predittori come "provide care" non spiega di più, ma proviamo ad aggiungere altri paesi

#Si prende come riferimento un uomo di 50 anni, che si percepisce in buona salute, svolge volontariato, 
#ha un livello educativo medio, vive in Italia e abita in una metropoli
regression_2<-lm(casp ~  age_cen + gender + poor + fair + very_good + excellent + volunteer +  religious + sport + Greece + Spain +  Netherlands + Denmark + Sweden + low_level + high_level + outskirts + large_town + small_town + village, data = share_w2)
summary(regression_2)

#Call:
#  lm(formula = casp ~ age_cen + gender + poor + fair + very_good + 
#       excellent + religious + sport + Greece + Spain + Netherlands + 
#       Denmark + Sweden + low_level + high_level + outskirts + large_town + 
#       small_town + village, data = share_w2)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-27.4984  -3.3885   0.2589   3.6446  18.7543 

Estimate Std. Error t value Pr(>|t|)    
(Intercept) 43.29799    0.55459  78.072  < 2e-16 ***
  age_cen     -0.03470    0.00304 -11.415  < 2e-16 ***
  gender      -0.51421    0.05696  -9.027  < 2e-16 ***
  poor        -7.18020    0.10252 -70.039  < 2e-16 ***
  fair        -2.62412    0.07382 -35.549  < 2e-16 ***
  very_good    1.48780    0.08067  18.442  < 2e-16 ***
  excellent    2.65193    0.10669  24.856  < 2e-16 ***
  volunteer    0.81586    0.09060   9.005  < 2e-16 ***
  religious    0.47894    0.09004   5.319 1.05e-07 ***
  sport        0.82972    0.07460  11.123  < 2e-16 ***
  Italy       -2.85404    0.10668 -26.753  < 2e-16 ***
  Greece      -3.38083    0.10532 -32.101  < 2e-16 ***
  Spain       -0.45803    0.12062  -3.797 0.000147 ***
  Netherlands  2.52769    0.11333  22.304  < 2e-16 ***
  Denmark      1.89509    0.11489  16.496  < 2e-16 ***
  Sweden       1.05490    0.11043   9.553  < 2e-16 ***
  low_level   -1.04373    0.06808 -15.332  < 2e-16 ***
  high_level   0.14694    0.08170   1.799 0.072107 .  
outskirts    0.22619    0.09852   2.296 0.021695 *  
  large_town   0.29774    0.09068   3.283 0.001027 ** 
  small_town   0.50487    0.08890   5.679 1.37e-08 ***
  village      0.18236    0.08492   2.148 0.031760 * 
#Residual standard error: 5.194 on 33380 degrees of freedom
#(3752 observations deleted due to missingness)
#Multiple R-squared:  0.3253,	Adjusted R-squared:  0.3249 
#F-statistic: 846.9 on 19 and 33380 DF,  p-value: < 2.2e-16

#LETà NON HA UN EFFETTO LINEARE SULLA QUALITà DELLA VITA!

#Si crea un EFFETTO DI INTERAZIONE da aggiundere alla regressione
share_w2$genVol = share_w2$gender*share_w2$volunteer
#è significativo ma non aggiunge nulla al modello

confint(regression_2)

vif(regression_2)
#diagnostica di multicollinearità

#age_sixty age_seventy  age_eighty      gender        poor        fair   very_good   excellent   religious 
#1.294870    1.334382    1.207481    1.020516    1.197159    1.289408    1.277821    1.192209    1.047134 

#sport      Greece       Spain    Netherlands     Denmark      Sweden   low_level  high_level   outskirts 
#1.102030    1.161297    1.106022    1.091058    1.101540    1.081085    1.422356    1.304460    1.485999 

#large_town  small_town     village 
#1.555252    1.630728    1.774785 

#testiamo la bontà della regressione
##Outliers: Residuals can be obtained with the resid() function
# come si distribuiscono i residui? dove sono i valori outliers?
resid(regression_2)
#MA non standardizzati, difficile compararli tra modelli perche' espressi
# in unita' di misura della VD

rstandard(regression_2)
standardized.residuals <-rstandard(regression_2)
# residui standardizzati per la stima della loro deviazione standard
# il 95% dei valori dovrebbero stare tra -1.96 e +1.96
# il 99% dei valori dovrebbero stare tra -2.58 e +2.58
# il 99,9% dei valori dovrebbero stare tra -3.29 e +3.29
# PREOCCUPANTI tutti i valori sopra il 3
large.residual <- standardized.residuals > 3.3 | standardized.residuals < -3.3
sum(large.residual)
#[1] 52 --> ci sono 52 valori che hanno una ds assoluta di 3.3

rstudent(regression_2)
# come i residui standardizzati ma la standardizzazione e' piu specifica
# rispetto ai valori puntuali (divisione per)

#Influential cases: Cook's distances can be obtained with the cooks.distance() function, 
# DFBeta with the dfbeta() function, DFFit with the dffits() function, 
# hat values (leverage) with the hatvalues() function, and the covariance ratio with the covratio() function.
cooks.distance(regression_2)
cooks.distance<-cooks.distance(regression_2)
cooks <- cooks.distance > 1
sum(cooks)
#[1] 0 --> non ci sono outliers nello spazio x-y
#Problema se Cook's distance >1


dfbeta(regression_2)
dfbeta <- dfbeta(regression_2)
2/sqrt(37152)
#[1] 0.01037621
dfbetacontrollo <- dfbeta>0.010
sum(dfbetacontrollo)
#[1] 45
#La soglia per DFBETAs e' 2/sqrt(n)

leverage <- hatvalues(regression_2)
leveragecontrollo<- leverage > 0.0017
sum(leveragecontrollo)
#[1] 0
#leverage > 2(k+1)/n o > 3(k+1)/n (K = predittori, n = osservazioni)
#3(21 + 1)/37152 (K=21, n=37152) ---> 0.0017
#Ci attendiamo una proporzione di .001 di casi nell'intervallo -3.3 to +3.3
#(i.e., 1/1000) à casi con |z|>3.3 sono da considerarsi outliers
