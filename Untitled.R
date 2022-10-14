install.packages("ggplot2")
library(ggplot2)
setwd("~/Desktop")
getwd()
indiv_data<-read.csv("~/Desktop/individual data new4.csv",header=TRUE) 
## Inconsistent(negative) and missing dates are encoded NA by as.Date
anyNA(indiv_data)
indiv_data<-na.exclude(indiv_data)
## We will first analyse sex vs the death rate  then age vs the death rate should be represented as an object of class rate in R. Inconsistent(negative) and missing dates are encoded NA by as.Date
## We can find the death ratio by find the death date.
qplot(age,death.ratio,data=indiv_data)
qplot(age,(death.ratio)^(1/3),data=indiv_data)
qplot(age,(death.ratio)^(1/3),data=indiv_data,geom=c("point","smooth"))
qplot(age,(death.ratio)^(1/5), data=indiv_data)+geom_smooth(method='lm', se=F)

