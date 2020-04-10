setwd("C:/Users/user/Desktop/R DIRECTORY")
library(readxl)
library(readr)
library(tidyxl)
library(data.table)
library(dplyr)
library(tidyverse)
philo<-read.csv("survival data.csv",header = TRUE)
philo
philo1<-data.table(philo)
philo1
str(philo1)
philo2<-na.omit(philo1)
philo2
philo3<-philo2[,.(hiv_status,tbhistory,outcome,time)]
philo3

philo3$outcome<-as.numeric(as.integer(philo3$outcome))
philo3$time<-as.numeric(as.factor(philo3$time))
library(survival)
Surv(philo3$time,philo3$outcome)
survfit(Surv(philo3$time,philo3$outcome)~1,data=philo3)
summary(survfit(Surv(philo3$time,philo3$outcome)~1,data=philo3))
par(mfrow=c(1,3))
p1<-plot(survfit(Surv(philo3$time,philo3$outcome)~1),main="survival curve",xlab='t',ylab='s(t)',col="green")
philo4<-philo3[hiv_status=='Pos']
philo4  
Surv(philo4$time,philo4$outcome)
survfit(Surv(philo4$time,philo4$outcome)~1,data=philo4)
summary(survfit(Surv(philo4$time,philo4$outcome)~1,data=philo4))
p2<-plot(survfit(Surv(philo4$time,philo4$outcome)~1),main="survival curve for hiv pos",xlab='t',ylab='s(t)',col = 'purple')
philo5<-philo3[hiv_status=='Neg']  
philo5
Surv(philo5$time,philo5$outcome)
survfit(Surv(philo5$time,philo5$outcome)~1,data = philo5)
summary(survfit(Surv(philo5$time,philo5$outcome)~1,data = philo5))
p3<-plot(survfit(Surv(philo5$time,philo5$outcome)~1),main="survival curve for hiv neg",xlab = 't',ylab='s(t)',col = 'blue')

