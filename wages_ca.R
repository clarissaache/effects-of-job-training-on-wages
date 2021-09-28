library(stargazer)
#library(tidyverse)
library(ggplot2)
library(arm)
library(pROC)
library(caret)
library(e1071)
library(GGally)

# ---------------------------- DATA SET UP ----------------------------

wages <- read.table("./Data//lalondedata.txt",sep=",",header=TRUE)
summary(wages)
wages$nonZero78 <- ifelse(wages$re78 > 0,1,0)
wages$nonZero78_fac <-factor(wages$nonZero78,levels=c(0,1),labels=c("Zero","Non-zero"))
wages$treat_fac <-factor(wages$treat,levels=c(0,1),labels=c("Not-treated","Treated"))
wages$black_fac <- factor(wages$black,levels=c(0,1),labels=c("Not Black","Black"))
wages$hispan_fac <- factor(wages$hispan,levels=c(0,1),labels=c("Not Hispanic","Hispanic"))
wages$married_fac <- factor(wages$married,levels=c(0,1),labels=c("Unmarried","Married"))
wages$nodegree_fac <- factor(wages$nodegree,levels=c(0,1),labels=c("degree","nodegree"))

wages$raceother <- wages$black==0 &wages$hispan==0
wages$raceother <- ifelse(wages$raceother,1,0)
wages$raceother_fac <-factor(wages$raceother,levels=c(0,1),labels=c("Black or Hispanic","Other race"))

wages$age_c <- wages$age-mean(wages$age)


# distribution of the reponse variable


hist(wages$re78,xlab="wage in 1978 (oz)",main="Distribution of wage in 1978",col=rainbow(10))
#not normal, take log
wages$logre78 <- log(wages$re78)
hist(wages$logre78,xlab="wage in 1978 (oz)",main="Distribution of log wage in 1978",col=rainbow(10))
