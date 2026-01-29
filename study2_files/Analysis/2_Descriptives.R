############### Descriptives NSSI EXPERIMENT 2 ##############
## Last update: 01/02/2021 ##
## R version 3.6.2 (2019-12-12) ##

##### Load libraries ####

library(Hmisc)
library(psy)
library(userfriendlyscience)
library(CTT)
library(psych)

##### 1. Read in final data-file ####

setwd("C:/Users/fcathely/Documents/NSSI_Study2/NSSI main")
Data<-read.table("Data1.txt") 
describe(Data) 

###### devide sample ###### 

#sample at baseline 
Data1 <- Data

#sample at follow-up 

Data2<-Data[!is.na(Data$prospective),]

##### 2. Demographics ###### 

#age 

mean(Data1$age, na.rm=T) 
sd(Data1$age, na.rm=T) 

mean(Data2$age, na.rm=T) 
sd(Data2$age, na.rm=T) 

#gender 

Data1$gender <- as.factor(Data1$gender)
table(Data1$gender)

Data2$gender <- as.factor(Data2$gender)
table(Data2$gender)

##### 3. internal consitency #### 

#Split-half reliability IAT  

cor.test(Data$IAT_odd, Data$IAT_even) 
spearman.brown(.55, input = 2, n.or.r = "n")  

#Cronbach's Alpha BHS 

Data3 <- Data[,8:27]
alpha(Data3)
