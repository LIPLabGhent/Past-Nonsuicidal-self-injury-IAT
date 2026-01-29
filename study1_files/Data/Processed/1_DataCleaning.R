############### DATA PREPARATION NSSI Experiment 1 ##############
## Last update: 27/01/2021 ##
## R version 3.6.2 (2019-12-12) ##

##### Load libraries ####

library('Hmisc')

##### 1. Read in Raw Data ####

#read in data

setwd("C:/Users/fcathely/Documents/NSSI_Study1/NSSI main")
Data<-read.csv("rawdata.csv", header=TRUE, sep=',')
head(Data)
describe(Data$observation) #94 pps

##### 2. Make Aggregated data file with questionnaire responses ####

Data <- unique(Data)
labels(Data)

Data1<-Data[!is.na(Data$gender),] # gender #1 = female, 2 = male, 3 = X 
DataAgg<-Data1[c(18,1)]
head(DataAgg)

# age 
Data1<-Data[!is.na(Data$age),]
Data1<-Data1[c(19,1)]
DataAgg<-merge(DataAgg,Data1, by="observation")
head(DataAgg)

#past NSSI year 

Data1<-Data[!is.na(Data$year),]
Data1<-Data1[c(24,1)]
DataAgg<-merge(DataAgg,Data1, by="observation")
head(DataAgg) 
describe(DataAgg) #88 

#past NSSI month 

Data1<-Data[!is.na(Data$month),]
Data1<-Data1[c(25,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)
describe(DataAgg)

#future NSSI  

Data1<-Data[!is.na(Data$likelihood),]
Data1<-Data1[c(26,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

#Hopelessness 

labels(Data)
Data1<-Data[!is.na(Data$s1),]
Data1<-Data1[c(27,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s2),]
Data1<-Data1[c(28,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s3),]
Data1<-Data1[c(29,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s4),]
Data1<-Data1[c(30,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s5),]
Data1<-Data1[c(31,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s6),]
Data1<-Data1[c(32,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s7),]
Data1<-Data1[c(33,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s8),]
Data1<-Data1[c(34,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s9),]
Data1<-Data1[c(35,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s10),]
Data1<-Data1[c(36,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s11),]
Data1<-Data1[c(37,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s12),]
Data1<-Data1[c(38,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s13),]
Data1<-Data1[c(39,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s14),]
Data1<-Data1[c(40,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s15),]
Data1<-Data1[c(41,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s16),]
Data1<-Data1[c(42,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s17),]
Data1<-Data1[c(43,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s18),]
Data1<-Data1[c(44,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s19),]
Data1<-Data1[c(45,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

Data1<-Data[!is.na(Data$s20),]
Data1<-Data1[c(46,1)]
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

DataAgg$BHS <- rowSums(DataAgg[,7:26], na.rm=TRUE)

#st

Data1<-Data[!is.na(Data$st),]
Data1<-Data1[c(47,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

#md 

labels(Data)

Data1<-Data[!is.na(Data$md),]
Data1<-Data1[c(48,1)]
DataAgg<-merge(DataAgg, Data1, by="observation")
head(DataAgg)

##### 3. Rearrange IAT Data ####

## only keep useful columns

Data<-Data[c(1,2,4,8,23)]
labels(Data)
Data<-Data[Data$sender=='Stimulus',]
describe(Data$observation) #93 pps

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

## only keep critical blocks 3,4,6,7

Data<-Data[Data$block>2&Data$block<8,]
Data<-Data[Data$block>5|Data$block<5,] 

Data$correct_<-ifelse(Data$correct=='TRUE',1,0)

## rename columns
head(Data)
Data<-Data[c(1,4,6:8)] 
Data<-Data[,c(1,3,4,5,2)] 
colnames(Data)=c("observation", "blocknumber", "trialnumber","correct", "latency") #rename columnnames 
summary(Data$latency)

#exclude pps with uncompleted IATs (96 trials)
ar<-table(Data$observation)
c1<-as.data.frame(ar)
c2<-c1[!c1$Freq==96,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data)  

##### 3. do IAT data cutting #######

## Discard data if errorrates >.30: more than 29
table(Data$observation,Data$correct)
ar<-table(Data$observation,Data$correct)
c1<-as.data.frame(ar)
c1<-c1[c1$Var2==0,]
c2<-c1[c1$Freq>29,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data)  #86 ppn, 2 excluded 

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
describe(Data)   #85 pps, 1 excluded 

## Discard data if fast trials: >10% (faster than 300 ms: more than 9)

Dataex<-Data[Data$latency<300,]
ar<-table(Dataex$observation)
c1<-as.data.frame(ar)
c2<-c1[c1$Freq>9,]
Data<-Data[!(Data$observation %in% c2$Var1),]
describe(Data) #84 pps, 1 excluded 

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

##### 5. Merge questionnaire-scores & IAT-scores #######

DataAgg$subject<-as.numeric(DataAgg$observation)
DataAgg<-merge(DataAgg,Data_, by="subject") #84

##### 6. write away final data file #######

write.table(DataAgg,"Data1.txt", sep="\t")
