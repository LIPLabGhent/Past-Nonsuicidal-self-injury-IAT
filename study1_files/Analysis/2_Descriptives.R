############### DATA PREPARATION NSSI EXPERIMENT 1: Descriptives ##############
## Last update: 27/01/2021 ##
## R version 3.6.2 (2019-12-12) ##

##### Load libraries ####

library(Hmisc)
library(psy)
library(userfriendlyscience)
library(CTT)
library(psych)

##### 1. Read in final data-file ####

setwd("C:/Users/fcathely/Documents/NSSI_Study1/NSSI main")
Data<-read.table("Data1.txt") 
describe(Data) 

##### 2. Demographics ###### 

#age 

mean(Data$age, na.rm=T) 
sd(Data$age, na.rm=T) 

#gender 

Data$gender <- as.factor(Data$gender)
table(Data$gender)

##### 3. internal consitency #### 

#Split-half reliability IAT  

cor.test(Data$IAT_odd, Data$IAT_even) 
spearman.brown(.57, input = 2, n.or.r = "n")  

#Cronbach's Alpha BHS 

Data1 <- Data[,8:27]
alpha(Data1)
