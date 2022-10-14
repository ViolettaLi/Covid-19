## individaul data from Kaggle
setwd("~/Desktop")
getwd()
indiv_data<-read.csv("~/Desktop/individual data new.csv",header=TRUE) 
## Inconsistent(negative) and missing dates are encoded NA by as.Date
anyNA(indiv_data)
indiv_data<-na.exclude(indiv_data)
## We will first analyse sex vs the death rate  then age vs the death rate should be represented as an object of class rate in R. Inconsistent(negative) and missing dates are encoded NA by as.Date
## We can find the death ratio by find the death date.
indiv_data$sex [indiv_data$sex %in% c("female","Female")] = "female"
indiv_data$sex [indiv_data$sex %in% c("male","Male")] ="male"
indiv_data$sex = factor(indiv_data$sex)
##Create a mosaic plot of sex versus death

install.packages("epiDisplay")
install.packages("vcd")
library(epiDisplay)
library(vcd)
attach(indiv_data)

xtab1=table(death,sex)
xtab1
mosaic(xtab1)

##Create a mosaic plot of age versus death
xtab2=table(death,age)
xtab2
mosaic(xtab2)

##Create a mosaic plot of sex and age versus death
xtab3=table(sex,age,death)
xtab3
mosaic(xtab3)

mosaic(table(death,sex),
       highlighting = "death",
       highlighting_fill = c("royal blue","gold"))

mosaic(table(death,age),
       highlighting = "death",
       highlighting_fill = c("royal blue","gold"))

mosaic(table(sex,age,death),
       highlighting = "death",
       highlighting_fill = c("royal blue","gold"))

mosaic(xtab1,shade=TRUE,legend=TRUE)
      
mosaic(xtab2,shade=TRUE,legend=TRUE)

mosaic(xtab3,shade=TRUE,legend=TRUE)
