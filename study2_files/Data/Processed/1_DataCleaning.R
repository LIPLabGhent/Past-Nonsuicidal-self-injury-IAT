############### DATA PREPARATION NSSI Experiment 2 ##############
## Last update: 01/02/2021 ##
## R version 3.6.2 (2019-12-12) ##

##### Load libraries ####

library('Hmisc')
library('tidyr')
library(tidyverse)

##### 1. Read in Raw Data ####

#read in data

setwd("C:/Users/fcathely/Documents/NSSI_Study2/main")
Data<-read.csv("RawData.csv", header=TRUE, sep=',')
head(Data)
describe(Data$observation) #485 pps

##### 2. Make Aggregated data file with questionnaire responses ####

labels(Data)

# gender 
Data1<-Data[!is.na(Data$gender),]
DataAgg<-Data1[c(5,1)]
head(DataAgg)

# age 
Data1<-Data[!is.na(Data$age),]
Data1<-Data1[c(6,1)]
DataAgg<-merge(DataAgg,Data1, by="observation")
head(DataAgg) #479

#past NSSI year 

Data1<-Data[!is.na(Data$year),]
Data1<-Data1[c(8,1)]
DataAgg<-merge(DataAgg,Data1, by="observation")
head(DataAgg) 
describe(DataAgg) #408

#past NSSI month 

Data1<-Data[!is.na(Data$month),]
Data1<-Data1[c(9,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)
describe(DataAgg)

#future likelihood NSSI  

Data1<-Data[!is.na(Data$likelihood),]
Data1<-Data1[c(10,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

#Hopelessness 

labels(Data)
Data1<-Data[!is.na(Data$s1),]
Data1<-Data1[c(11,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s2),]
Data1<-Data1[c(12,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s3),]
Data1<-Data1[c(13,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s4),]
Data1<-Data1[c(14,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s5),]
Data1<-Data1[c(15,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s6),]
Data1<-Data1[c(16,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s7),]
Data1<-Data1[c(17,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s8),]
Data1<-Data1[c(18,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s9),]
Data1<-Data1[c(19,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s10),]
Data1<-Data1[c(20,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s11),]
Data1<-Data1[c(21,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s12),]
Data1<-Data1[c(22,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s13),]
Data1<-Data1[c(23,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s14),]
Data1<-Data1[c(24,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s15),]
Data1<-Data1[c(25,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s16),]
Data1<-Data1[c(26,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s17),]
Data1<-Data1[c(27,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s18),]
Data1<-Data1[c(28,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s19),]
Data1<-Data1[c(29,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s20),]
Data1<-Data1[c(30,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

#reversed scoring items BHS 

old <- 0:1 
new <-c(1,0) 

DataAgg$s1[DataAgg$s1 %in% old] <- new[match(DataAgg$s1, old, nomatch = 0)]
DataAgg$s3[DataAgg$s3 %in% old] <- new[match(DataAgg$s3, old, nomatch = 0)]
DataAgg$s5[DataAgg$s5 %in% old] <- new[match(DataAgg$s5, old, nomatch = 0)]
DataAgg$s6[DataAgg$s6 %in% old] <- new[match(DataAgg$s6, old, nomatch = 0)]
DataAgg$s8[DataAgg$s8 %in% old] <- new[match(DataAgg$s8, old, nomatch = 0)]
DataAgg$s10[DataAgg$s10 %in% old] <- new[match(DataAgg$s10, old, nomatch = 0)]
DataAgg$s13[DataAgg$s13 %in% old] <- new[match(DataAgg$s13, old, nomatch = 0)]
DataAgg$s15[DataAgg$s15 %in% old] <- new[match(DataAgg$s15, old, nomatch = 0)]
DataAgg$s19[DataAgg$s19 %in% old] <- new[match(DataAgg$s19, old, nomatch = 0)]

#totalscores BHS 

DataAgg$BHS <- rowSums(DataAgg[,7:26], na.rm=TRUE) #408

#st

Data1<-Data[!is.na(Data$st),]
Data1<-Data1[c(31,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

#md 

labels(Data)

Data1<-Data[!is.na(Data$md),]
Data1<-Data1[c(32,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg) #407

#prospective 

Data1<-Data[!is.na(Data$prospective),] 
Data1<-Data1[c(33,1)]
DataAgg <- merge(DataAgg, Data1, all=TRUE) 
DataAgg<- DataAgg[complete.cases(DataAgg[, 2:29]), ]

sum(is.na(DataAgg$prospective)) #82

##### 3. Rearrange IAT Data ####

## only keep useful columns

Data<-Data[c(1,2,3,4,7)]
labels(Data)
Data<-Data[Data$sender=='Stimulus',]
describe(Data$observation) #458

## create trial and blocknumbers

Data$block<-ifelse(Data$sender_id=='6_3_0_0_1',1,0)
Data$trial<-ifelse(Data$sender_id=='6_3_0_0_1',1,0)

for(i in 1:23){
  out <- paste("6_3_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,1,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_5_0_0_1',2,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_5_0_0_1',1,Data$trial)

for(i in 1:23){
  out <- paste("6_5_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,2,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_7_0_0_1',3,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_7_0_0_1',1,Data$trial)

for(i in 1:15){
  out <- paste("6_7_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,3,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_9_0_0_1',4,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_9_0_0_1',1,Data$trial)

for(i in 1:31){
  out <- paste("6_9_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,4,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_11_0_0_1',5,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_11_0_0_1',1,Data$trial)

for(i in 1:23){
  out <- paste("6_11_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,5,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_13_0_0_1',6,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_13_0_0_1',1,Data$trial)

for(i in 1:15){
  out <- paste("6_13_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,6,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

Data$block<-ifelse(Data$sender_id=='6_15_0_0_1',7,Data$block)
Data$trial<-ifelse(Data$sender_id=='6_15_0_0_1',1,Data$trial)

for(i in 1:31){
  out <- paste("6_15_", i, "_0_1", sep="")
  Data$block<-ifelse(Data$sender_id==out,7,Data$block)
  Data$trial<-ifelse(Data$sender_id==out,i+1,Data$trial)
  i=i+1
}

table(Data$observation,Data$block)
describe(Data$observation)

## only keep critical blocks 3,4,6,7

Data<-Data[Data$block>2&Data$block<8,]
Data<-Data[Data$block>5|Data$block<5,] 

Data$correct_<-ifelse(Data$correct=='TRUE',1,0)

describe(Data$observation)

## rename columns
head(Data)
Data<-Data[c(1,4,6:8)] 
Data<-Data[,c(1,3,4,5,2)] 
colnames(Data)=c("observation", "blocknumber", "trialnumber","correct", "latency") #rename columnnames 
summary(Data$latency)
Data<-Data[!is.na(Data$latency),]

describe(Data) #448

#exclude pps with uncompleted IATs (96 trials)
ar<-table(Data$observation)
c1<-as.data.frame(ar)
c2<-c1[!c1$Freq==96,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data$observation)  #408 ppn

##### 3. do IAT data cutting #######

## Discard data if errorrates >.30: more than 29
table(Data$observation,Data$correct)
ar<-table(Data$observation,Data$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>29,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data$observation)  #399 ppn, 9 excluded 

## Discard data if errorrates >.40 for any block: more than 7/13
D1<-Data[Data$blocknumber==3,]
ar<-table(D1$observation,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>7,]
Data<-Data[!(Data$observation %in% c2$Var1),]

D1<-Data[Data$blocknumber==4,]
ar<-table(D1$observation,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>13,]
Data<-Data[!(Data$observation %in% c2$Var1),]

D1<-Data[Data$blocknumber==6,]
ar<-table(D1$observation,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>7,]
Data<-Data[!(Data$observation %in% c2$Var1),]

D1<-Data[Data$blocknumber==7,]
ar<-table(D1$observation,D1$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>13,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data$observation)   #378 ppn,21 excluded 

## Discard data if fast trials: >10% (faster than 300 ms: more than 9)

Dataex<-Data[Data$latency<300,]
ar<-table(Dataex$observation)
c1<-as.data.frame(ar)
c2<-c1[c1$Freq>9,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data$observation) #375 ppn, 3 excluded 

##### 4. compute IAT D4 scores #######

## Run script to calculate D4 IAT score for aIAT
head(Data)
colnames(Data)=c("subject", "blocknumber", "trialnumber","correct", "latency") #rename columnnames 
Data$observation<-Data$subject
Data$subject<-as.numeric(Data$subject)

VAR=c(1,2,3,4,5)
exclude=c()
lower.bound=0
Data<- Data[order(Data$subject),]
summary(Data)
source("maarten_iatscript.txt")
out <- IAT(Data, VAR, exclude, lower.bound)
summary(out)

## Check plot
plot(out, d="D4")
plot(out$score$D4)

## Calculate D4 score
Data_<- out$score$Subject
Data_<-cbind(Data_, out$score$D4)
colnames(Data_)=c("subject", "IAT")
head(Data_) 

## Calculate D4 score for odd-numbered trials
Datao<- Data[Data$trialnumber%%2==1,]
table(Datao$trialnumber)
out<-IAT(Datao,VAR,exclude,lower.bound)
Data_<-cbind(Data_, out$score$D4)
colnames(Data_)=c("subject", "IAT","IAT_odd")

## Calculate D4 score for even-numbered trials
Datae<- Data[Data$trialnumber%%2==0,]
Datae<- Datae[order(Datae$observation),]
out<-IAT(Datae,VAR,exclude,lower.bound)
Data_<-data.frame(Data_, out$score$D4)
colnames(Data_)=c("subject", "IAT","IAT_odd","IAT_even")

describe(Data_$subject)

##### 5. Merge questionnaire-scores & IAT-scores #######

DataAgg$subject<-as.numeric(DataAgg$observation)
DataAgg<-merge(DataAgg,Data_, by="subject") #374

##### 6. write away final data file #######

write.table(DataAgg,"Data1.txt", sep="\t")
