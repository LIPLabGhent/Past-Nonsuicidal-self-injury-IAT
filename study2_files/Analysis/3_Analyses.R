############### Analyses NSSI EXPERIMENT 2 ##############
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

setwd('C:/Users/fcathely/Documents/NSSI_Study2/Main')
Data<-read.table("Data1.txt", header=TRUE, sep = "\t")  
describe(Data) 

Data <- Data[!Data$observation=="576bf",] #indicated to have engaged in NSSI in past month but not in past year 
Data <- Data[!Data$observation=="f0381",]

##### 2. t-tests: discrimination pps ##### 

##Group participants  

Data1<-Data[Data$year>=1,] 

Data2<-Data[which(Data$year=="0" & Data$month =="0"),] 

Data3<-Data[Data$month>=1,] 

Data4<-Data[Data$prospective>=1,]
Data4<- Data4[complete.cases(Data4), ]
Data5<-Data[Data$prospective==0,]
Data5<- Data5[complete.cases(Data5), ]

##t-test 12 months 

t.test(Data1$IAT, Data2$IAT) 
cohensD(Data1$IAT, Data2$IAT) 
sd(Data1$IAT) 
sd(Data2$IAT) 

##t-test month 

t.test(Data3$IAT, Data2$IAT) 
cohensD(Data3$IAT, Data2$IAT) 
sd(Data3$IAT) 
sd(Data2$IAT) 

##t-test prospective 

t.test(Data4$IAT, Data5$IAT) 
cohensD(Data4$IAT, Data5$IAT) 
sd(Data4$IAT) 
sd(Data5$IAT)

##### 3. Classification analyses ####

###Group injurers and non-injurers 

#year 
Data1 <- Data
Data1$NSSIyear <- ifelse((Data$year==0 & Data$month==0), 0, 1)
Data1$NSSIyear <- ifelse((Data1$year==0 & Data1$month>0), 2, Data1$NSSIyear)
Data1 <- Data1[!Data1$NSSIyear==2, ] 
table(Data1$NSSIyear)

#month 
Data2<-Data 
Data2$NSSImonth <- ifelse((Data$year==0 & Data$month==0), 0, 1)
Data2$NSSImonth <- ifelse((Data2$year>0 & Data2$month==0), 2, Data2$NSSImonth)
Data2 <- Data2[!Data2$NSSImonth==2, ] 
table(Data2$NSSImonth)

#prospective 
Data3<-Data 
Data3$NSSIpros <- ifelse((Data$prospective==0), 0, 1)
Data3<- Data3[complete.cases(Data3), ]
table(Data3$NSSIpros)

###Factorize variables 

Data1$NSSIyear <- as.factor(Data1$NSSIyear)
Data2$NSSImonth <- as.factor(Data2$NSSImonth)
Data3$NSSIpros <- as.factor(Data3$NSSIpros)

###AUC

## Year 

#estimated probabilities 
plot(x=Data1$IAT, y=Data1$NSSIyear)
glm.fit <- glm(Data1$NSSIyear~Data1$IAT, family=binomial) 
lines(Data1$IAT,glm.fit$fitted.values)

#draw roc-curve 
roc(Data1$NSSIyear, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, ci=TRUE)  

#cross-validated AUC 

Data1a <- Data1

levels(Data1a$NSSIyear) <- c("first_class", "second_class")

set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(NSSIyear ~ IAT, data = Data1a, method = "glm", family = "binomial", metric = "ROC",
               trControl = train.control)

print(model) 

##Month 

#estimated probabilities 
plot(x=Data2$IAT, y=Data2$NSSImonth)
glm.fit <- glm(Data2$NSSImonth~Data2$IAT, family=binomial) 
lines(Data2$IAT,glm.fit$fitted.values)

#draw roc-curve 
roc(Data2$NSSImonth, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, ci=TRUE)

#cross-validated AUC 

Data2a <- Data2

levels(Data2a$NSSImonth) <- c("first_class", "second_class")
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the model
model <- train(NSSImonth ~ IAT, data = Data2a, method = "glm", family = "binomial", metric = "ROC",
               trControl = train.control)

# Summarize the results
print(model) 

##Prospective

#estimated probabilities 
plot(x=Data3$IAT, y=Data3$NSSIpros)
glm.fit <- glm(Data3$NSSIpros~Data3$IAT, family=binomial) 
lines(Data3$IAT,glm.fit$fitted.values)

#draw roc-curve 
roc(Data3$NSSIpros, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, ci=TRUE) 

# cross-validated AUC 

Data3a <- Data3

levels(Data3a$NSSIpros) <- c("first_class", "second_class")
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(NSSIpros ~ IAT, data = Data3a, method = "glm", family = "binomial", metric = "ROC",
               trControl = train.control)

print(model) 

### Sensitivity & specificity  

##year 

#cut-off: 0 

Data1$NSSIss1 <- ifelse((Data1$IAT>=0), yes=1, no=0)
Data1$NSSIss1 <-factor(Data1$NSSIss1)
class(Data1$NSSIss1)
levels(Data1$NSSIss1)

pairs <- data.frame(pred=factor(unlist(Data1[36])), ref=factor(Data1[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off: .160  

Data1$NSSIss2 <- ifelse((Data1$IAT>=.160), yes=1, no=0)
Data1$NSSIss2<-factor(Data1$NSSIss2)
class(Data1$NSSIss2)
levels(Data1$NSSIss2)

pairs <- data.frame(pred=factor(unlist(Data1[37])), ref=factor(Data1[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

# cut-off: .650 

Data1$NSSIss3 <- ifelse((Data1$IAT>=.650), yes=1, no=0) 
Data1$NSSIss3<-factor(Data1$NSSIss3)
class(Data1$NSSIss3)
levels(Data1$NSSIss3)

pairs <- data.frame(pred=factor(unlist(Data1[38])), ref=factor(Data1[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

##month 

#cut-off: 0 

Data2$NSSIss1 <- ifelse((Data2$IAT>=0), yes=1, no=0)
Data2$NSSIss1 <-factor(Data2$NSSIss1)
class(Data2$NSSIss1)
levels(Data2$NSSIss1)

pairs <- data.frame(pred=factor(unlist(Data2[36])), ref=factor(Data2[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off: .160

Data2$NSSIss2 <- ifelse((Data2$IAT>=.160), yes=1, no=0)
Data2$NSSIss2 <-factor(Data2$NSSIss2)
class(Data2$NSSIss2)
levels(Data2$NSSIss2)

pairs <- data.frame(pred=factor(unlist(Data2[37])), ref=factor(Data2[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off: .650

Data2$NSSIss3 <- ifelse((Data2$IAT>=.650), yes=1, no=0)
Data2$NSSIss3 <-factor(Data2$NSSIss3)
class(Data2$NSSIss3)
levels(Data2$NSSIss3)

pairs <- data.frame(pred=factor(unlist(Data2[38])), ref=factor(Data2[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

##prospective 

#cut-off: 0 

Data3$NSSIss1 <- ifelse((Data3$IAT>=0), yes=1, no=0)
Data3$NSSIss1 <-factor(Data3$NSSIss1)
class(Data3$NSSIss1)
levels(Data3$NSSIss1)

pairs <- data.frame(pred=factor(unlist(Data3[36])), ref=factor(Data3[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off: .160

Data3$NSSIss2 <- ifelse((Data3$IAT>=.160), yes=1, no=0)
Data3$NSSIss2 <-factor(Data3$NSSIss2)
class(Data3$NSSIss2)
levels(Data3$NSSIss2)

pairs <- data.frame(pred=factor(unlist(Data3[37])), ref=factor(Data3[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

#cut-off: .650

Data3$NSSIss3 <- ifelse((Data3$IAT>=.650), yes=1, no=0)
Data3$NSSIss3 <-factor(Data3$NSSIss3)
class(Data3$NSSIss3)
levels(Data3$NSSIss3)

pairs <- data.frame(pred=factor(unlist(Data3[38])), ref=factor(Data3[,35])) 
pred<-pairs$pred
ref <- pairs$ref
confusionMatrix(pred, ref, positive="1") 

##### 4. Logistic regression: prediction ###### 

#year 

fit1 <- glm(NSSIyear ~ IAT, data = Data1, family = "binomial")
summary(fit1) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2)
exp(cbind(OR = coef(fit1), confint(fit1))) 

#month 

fit2 <- glm(NSSImonth ~ IAT, data = Data2, family = "binomial")
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

#prospective 

fit3 <- glm(NSSIpros ~ IAT, data = Data3, family = "binomial")
summary(fit3) 
wald.test(b = coef(fit3), Sigma = vcov(fit3), Terms = 2) 
exp(cbind(OR = coef(fit3), confint(fit3))) 

##### 5. Hierarchical Logistic regression: incremental validity ###### 

### factorize and transform  variables  

Data1$gender<-factor(Data1$gender)
Data2$gender<-factor(Data2$gender)
Data3$gender<-factor(Data3$gender)
Data$gender<-factor(Data$gender)

Data1$md<-factor(Data1$md)
Data2$md<-factor(Data2$md)
Data3$md<-factor(Data3$md)
Data$md<-factor(Data$md)

Data1$logst <- log(Data1$st+1)
Data2$logst <- log(Data2$st+1)
Data3$logst <- log(Data3$st+1)
Data$logst <- log(Data$st+1)

Data3$logyear <- log(Data3$year+1)
Data$logyear <- log(Data$year+1)

Data3$logmonth <- log(Data3$month+1)
Data$logmonth <- log(Data$month+1)

###year 

## test which risk factors are signifcant 

fit1 <- glm(NSSIyear ~ gender, data = Data1, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIyear ~ age, data = Data1, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIyear ~ md, data = Data1, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIyear ~ BHS, data = Data1, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIyear ~ logst, data = Data1, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

##test incremental validity 

fit1 <- glm(NSSIyear ~ gender+age+md+BHS+logst, data = Data1, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 5) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 6) 
exp(cbind(coef(fit1), confint(fit1))) 

fit2 <- glm(NSSIyear ~ gender+age+md+BHS+logst+IAT, data = Data1, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 6) 
exp(cbind(OR = coef(fit2), confint(fit2)))

lrtest(fit1)
lrtest(fit1, fit2) 
nagelkerke(fit1)
nagelkerke(fit2)

###month  

## test which risk factors are signifcant 

fit1 <- glm(NSSImonth ~ gender, data = Data2, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(NSSImonth ~ age, data = Data2, family = "binomial")
summary(fit1)  
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSImonth ~ md, data = Data2, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSImonth ~ BHS, data = Data2, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSImonth ~ logst, data = Data2, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))

##test incremental validity 

fit1 <- glm(NSSImonth ~ gender+age+BHS+logst, data = Data2, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 5) 
exp(cbind(coef(fit1), confint(fit1))) 

fit2 <- glm(NSSImonth ~ gender+age+BHS+logst+IAT, data = Data2, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 6) 
exp(cbind(OR = coef(fit2), confint(fit2)))

lrtest(fit1)
lrtest(fit1, fit2) 
nagelkerke(fit1)
nagelkerke(fit2)

###likelihood 

## test which risk factors are signifcant 

fit1 <- glm(dichlikely ~ gender, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(dichlikely ~ age, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(dichlikely ~ md, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ logst, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(dichlikely ~ BHS, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(dichlikely ~ logyear, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(dichlikely ~ logmonth, data = Data, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

##test incremental validity 

fit1 <- glm(dichlikely ~ age+BHS+logst+logyear+logmonth, data = Data, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 5) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 6) 
exp(cbind(coef(fit1), confint(fit1))) 

fit2 <- glm(dichlikely ~ age+BHS+logst+logyear+logmonth+IAT, data = Data, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 7) 
exp(cbind(OR = coef(fit2), confint(fit2)))

lrtest(fit1)
lrtest(fit1, fit2) 
nagelkerke(fit1)
nagelkerke(fit2)

### prospective NSSI 

## test which risk factors are signifcant 

fit1 <- glm(NSSIpros ~ gender, data = Data3, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIpros ~ age, data = Data3, family = "binomial")
summary(fit1)  
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIpros ~ md, data = Data3, family = "binomial")
summary(fit1)
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIpros ~ BHS, data = Data3, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(NSSIpros ~ logst, data = Data3, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

fit1 <- glm(NSSIpros ~ logyear, data = Data3, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1)))  

fit1 <- glm(NSSIpros ~ logmonth, data = Data3, family = "binomial")
summary(fit1) 
exp(cbind(coef(fit1), confint(fit1))) 

##test incremental validity

fit1 <- glm(NSSIpros ~ BHS+logst+logyear+logmonth, data = Data3, family = "binomial")
summary(fit1)
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 2) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 3) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 4) 
wald.test(b = coef(fit1), Sigma = vcov(fit1), Terms = 5) 
exp(cbind(OR = coef(fit1), confint(fit1))) 

fit2 <- glm(NSSIpros ~ BHS+logst+logyear+logmonth+IAT, data = Data3, family = "binomial")
summary(fit2)
wald.test(b = coef(fit2), Sigma = vcov(fit2), Terms = 6)
exp(cbind(OR = coef(fit2), confint(fit2))) 

lrtest(fit1)
lrtest(fit1, fit2)  
nagelkerke(fit1)
nagelkerke(fit2)