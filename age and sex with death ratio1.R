install.packages("rms")
library(rms)
## individaul data from Kaggle
setwd("~/Desktop")
getwd()
indiv_data<-read.csv("~/Desktop/individual data.csv",header=TRUE) 
## Inconsistent(negative) and missing dates are encoded NA by as.Date
anyNA(indiv_data)
indiv_data<-na.exclude(indiv_data)
## We will first analyse sex vs the death rate  then age vs the death rate should be represented as an object of class rate in R. Inconsistent(negative) and missing dates are encoded NA by as.Date
## We can find the death ratio by find the death date.
indiv_data$sex [indiv_data$sex %in% c("female","Female")] = "female"
indiv_data$sex [indiv_data$sex %in% c("male","Male")] ="male"
indiv_data$sex = factor(indiv_data$sex)
table(as.factor(indiv_data$sex))
attach(indiv_data)
## Fit the logistic regression model
mod1<-glm(death~sex,data=indiv_data,family=binomial)
##mod1
summary(mod1)
confint.default(mod1)
##get odds ratio
exp(coef(mod1))

##Now we will perform another logistic regression. Our variable of interest being age We need first to put the variable in the correct type. A warning message will appear for inconsistent entries.
indiv_data$age = as.numeric(as.character(indiv_data$age))
table(as.factor(indiv_data$age))
mod2<-glm(death~age,data=indiv_data,family=binomial)
##mod2
summary(mod2)
confint.default(mod2)
##get odds ratio
exp(coef(mod2))

mod3<-glm(death~sex+age,data=indiv_data,family=binomial)
##mod3
summary(mod3)
confint.default(mod3)
##get odds ratio
exp(coef(mod3))

##Create a boxplot of sex versus death
boxplot(death ~sex, data = indiv_data, xlab = "Sex", ylab = "if death")

##Create a boxplot of age versus death
boxplot(death ~age, data = indiv_data, xlab = "age", ylab = "if death")

##Filter the independent variables step by step
mod.none<-glm(death~1,data=indiv_data,family=binomial)
mod.full<-glm(death~.,data=indiv_data,family=binomial)
y.step<-step(mod.none,scope=formula(mod.full),direction="both",trace=F)
summary(y.step)
##choose mod2
age<-indiv_data$age
death<-indiv_data$death
sex<-indiv_data$sex


log.pre<-predict(y.step,data.frame(age=1))
p1<-exp(log.pre)/(1+exp(log.pre));p1

log.pre<-predict(y.step,data.frame(age=0))
p2<-exp(log.pre)/(1+exp(log.pre));p2





