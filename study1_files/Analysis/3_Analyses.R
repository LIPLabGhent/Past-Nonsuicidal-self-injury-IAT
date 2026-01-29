############### Analyses NSSI EXPERIMENT 1 ##############
## Last update: 08/07/2021 ##
## R version 3.6.2 (2019-12-12) ##

##### Load libraries ####

library(Hmisc)
library(psy)
library(car)
library(effects)
library(BayesFactor)
library(lsr)
library(Hmisc)
library(psy)
library(afex)
library(reshape)
library(effects)
library(car)
library(lme4)
library(pROC)
library(randomForest)
library(caret)
library(lmtest)
library(sandwich)
library(lmSupport)
library(e1071)
library(effectsize)
library(lsr)
library(effects)
library(aod)
library(survey)
library(pscl)
library(rcompanion)
library(cvAUC)
library(sail)

##### 1. Read in final data-file #####

setwd("C:/Users/fcathely/Documents/NSSI_Study1/NSSI main")
Data<-read.table("Data1.txt", header=TRUE, sep = "\t")  
describe(Data) 

Data<- Data[!Data$observation=="7144c",] #indicated to have engaged in NSSI in past month but not in past year 

##### 2. t-tests: discrimination pps  ##### 

#group participants 

Data1<-Data[Data$year>=1,] 
Data2<-Data[which(Data$year=="0" & Data$month =="0"),]
Data3<-Data[Data$month>=1,] 

#t-tests 

t.test(Data1$IAT, Data2$IAT) 
cohensD(Data1$IAT, Data2$IAT) 
sd(Data1$IAT)
sd(Data2$IAT)

t.test(Data3$IAT, Data2$IAT) 
cohensD(Data3$IAT, Data2$IAT) 
sd(Data3$IAT)
sd(Data2$IAT)

##### 3. Classification analyses  #####

###Group participants 

Data5<-Data 
Data5$NSSIyear <- ifelse((Data$year==0 & Data$month==0), 0, 1)
Data5$NSSIyear <- ifelse((Data5$year==0 & Data5$month>0), 2, Data5$NSSIyear)
Data5 <- Data5[!Data5$NSSIyear==2, ] 

Data6<-Data 
Data6$NSSImonth <- ifelse((Data$year==0 & Data$month==0), 0, 1)
Data6$NSSImonth <- ifelse((Data6$year>0 & Data6$month==0), 2, Data6$NSSImonth)
Data6 <- Data6[!Data6$NSSImonth==2, ] 

###Factorize variables 

Data5$NSSIyear <- as.factor(Data5$NSSIyear)
table(Data5$NSSIyear)
str(Data5$NSSIyear)

Data6$NSSImonth <- as.factor(Data6$NSSImonth)
table(Data6$NSSImonth)
str(Data6$NSSImonth)

###AUC

##Year 

#estimated probabilities 
plot(x=Data5$IAT, y=Data5$NSSIyear)
glm.fit <- glm(Data5$NSSIyear~Data5$IAT, family=binomial) 
lines(Data5$IAT,glm.fit$fitted.values)

#draw roc-curve 
roc(Data5$NSSIyear, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, ci=TRUE) 

#cross-validated AUC 

Data5a <- Data5

levels(Data5a$NSSIyear) <- c("first_class", "second_class")
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(NSSIyear ~ IAT, data = Data5a, method = "glm", family = "binomial", metric = "ROC",
               trControl = train.control)

print(model) 

##Month 

#estimated probabilities 
plot(x=Data6$IAT, y=Data6$NSSImonth)
glm.fit <- glm(Data6$NSSImonth~Data6$IAT, family=binomial) 
lines(Data6$IAT,glm.fit$fitted.values)

#draw roc-curve 
roc(Data6$NSSImonth, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, ci = TRUE)

#cross-validated AUC 

Data6a <- Data6

levels(Data6a$NSSImonth) <- c("first_class", "second_class")
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(NSSImonth ~ IAT, data = Data6a, method = "glm", family = "binomial", metric = "ROC",
               trControl = train.control)

print(model)

####Sensitivity & specificity 

##year 

#cut-off IAT:0 

Data5$NSSIss1 <- ifelse((Data5$IAT>=0), yes=1, no=0)
Data5$NSSIss1 <-factor(Data5$NSSIss1)
class(Data5$NSSIss1)
levels(Data5$NSSIss1)

pairs <- data.frame(pred=factor(unlist(Data5[35])), ref=factor(Data5[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive = "1") 

#cut-off IAT:.160

Data5$NSSIss2 <- ifelse((Data5$IAT>=.160), yes=1, no=0)
Data5$NSSIss2<-factor(Data5$NSSIss2)
class(Data5$NSSIss2)
levels(Data5$NSSIss2)

pairs <- data.frame(pred=factor(unlist(Data5[36])), ref=factor(Data5[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref,  positive="1") 

#cut-off IAT:.650

Data5$NSSIss3 <- ifelse((Data5$IAT>=.650), yes=1, no=0) 
Data5$NSSIss3<-factor(Data5$NSSIss3)
class(Data5$NSSIss3)
levels(Data5$NSSIss3)

pairs <- data.frame(pred=factor(unlist(Data5[37])), ref=factor(Data5[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

##month 

#cut-off IAT: 0 

Data6$NSSIss1 <- ifelse((Data6$IAT>=0), yes=1, no=0)

Data6$NSSIss1 <-factor(Data6$NSSIss1)
class(Data6$NSSIss1)
levels(Data6$NSSIss1)

pairs <- data.frame(pred=factor(unlist(Data6[35])), ref=factor(Data6[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off IAT: .160

Data6$NSSIss2 <- ifelse((Data6$IAT>=.160), yes=1, no=0)
Data6$NSSIss2 <-factor(Data6$NSSIss2)
class(Data6$NSSIss2)
levels(Data6$NSSIss2)

pairs <- data.frame(pred=factor(unlist(Data6[36])), ref=factor(Data6[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off IAT: .650

Data6$NSSIss3 <- ifelse((Data6$IAT>=.650), yes=1, no=0)
Data6$NSSIss3 <-factor(Data6$NSSIss3)
class(Data6$NSSIss3)
levels(Data6$NSSIss3)

pairs <- data.frame(pred=factor(unlist(Data6[37])), ref=factor(Data6[,34])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

##### 4. Logistic regression: independent prediction ###### 

#year 

fit1 <- glm(NSSIyear ~ IAT, data = Data5, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

#month 

fit2 <- glm(NSSImonth ~ IAT, data = Data6, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 2) 
exp(cbind(OR = coef(fit2), confint(fit2))) 

#likelihood 

Data$dichlikely <- ifelse((Data$likelihood==0 | Data$likelihood==1 | Data$likelihood==2), 0, 1)
table(Data$dichlikely)

fit2 <- glm(dichlikely ~ IAT, data = Data, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 2) 
exp(cbind(OR = coef(fit2), confint(fit2))) 

##### 5. Hierarchical Logistic regression: incremental validity ###### 

### factorize and transform continous variables 

Data5$gender<-factor(Data5$gender)
Data6$gender<-factor(Data6$gender)
Data$gender<-factor(Data$gender)

Data5$md<-factor(Data5$md)
Data6$md<-factor(Data6$md)
Data$md<-factor(Data$md)

Data5$logst <- log(Data5$st+1)
Data6$logst <- log(Data6$st+1)
Data$logst <- log(Data$st+1)
Data$logyear <- log(Data$year+1)
Data$logmonth <- log(Data$month+1)

###year 

Data5 <- Data5[!Data5$subject=="68", ] # only one pp gender = 3 --> inadequate sample size 
table(Data5$NSSIyear)

## test which risk factors are signifcant 

fit1 <- glm(NSSIyear ~ gender, data = Data5, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIyear ~ age, data = Data5, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(NSSIyear ~ md, data = Data5, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(NSSIyear ~ logst, data = Data5, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))

fit1 <- glm(NSSIyear ~ BHS, data = Data5, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

## test incremental validity 

fit1 <- glm(NSSIyear ~ gender+age+md+logst+BHS, data = Data5, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 5) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 6) 

exp(cbind(coef(fit1), confint(fit1))) 

fit2 <- glm(NSSIyear ~ gender+age+md+logst+BHS+IAT, data = Data5, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 7) 
exp(cbind(OR = coef(fit2), confint(fit2))) 

lrtest(fit1)
lrtest(fit1, fit2) 
nagelkerke(fit1)
nagelkerke(fit2)

###month 

fit1 <- glm(NSSImonth ~ gender, data = Data6, family = "binomial")
summary(fit1) 
exp(cbind(OR = coef(fit1), confint(fit1)))

fit1 <- glm(NSSImonth ~ age, data = Data6, family = "binomial")
summary(fit1) 
exp(cbind(OR = coef(fit1), confint(fit1)))

fit1 <- glm(NSSImonth ~ md, data = Data6, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(NSSImonth ~ logst, data = Data6, family = "binomial")
summary(fit1) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(NSSImonth ~ BHS, data = Data6, family = "binomial")
summary(fit1) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

#test incremental validity 

fit1 <- glm(NSSImonth ~ logst+BHS, data = Data6, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit2 <- glm(NSSImonth ~ logst+BHS+IAT, data = Data6, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 4) 
exp(cbind(OR = coef(fit2), confint(fit2))) 

lrtest(fit1)
lrtest(fit1, fit2)
nagelkerke(fit1)
nagelkerke(fit2)

###future likelihood 

## test which risk factors are signifcant 

Data <- Data[!Data$subject=="68", ] # only one pp gender = 3 --> inadequate sample size 

fit1 <- glm(dichlikely ~ gender, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ age, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ md, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ logst, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ BHS, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1)))

fit1 <- glm(dichlikely ~ logyear, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ logmonth, data = Data, family = "binomial")
summary(fit1)
exp(cbind(OR = coef(fit1), confint(fit1))) 

##test incremental validity 

fit1 <- glm(dichlikely ~ BHS+logyear+logmonth, data = Data, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit2 <- glm(dichlikely ~ BHS+logyear+logmonth+IAT, data = Data, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 5) 
exp(cbind(OR = coef(fit2), confint(fit2))) 

lrtest(fit1)
lrtest(fit1, fit2) 
nagelkerke(fit1)
nagelkerke(fit2)