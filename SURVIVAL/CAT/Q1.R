###QUESTION ONE
setwd("C:/Users/user/Desktop/R DIRECTORY")
library(survival)
library(ggplot2)
#group A<-c(63+, 59+, 57+, 40, 37, 33, 21+, 11)
Time1<-c(63, 59, 57, 40, 37, 33, 21, 11 )
Status1<-c(0,0,0,1,1,1,0,1)
Surv(Time1,Status1)
survfit(Surv(Time1,Status1)~1)
summary(survfit(Surv(Time1,Status1)~1))
m<-plot(survfit(Surv(Time1,Status1)~1),main="GROUP A",xlab = 't',ylab = 's(t)')

#group B<-c(: 57+, 51+, 44+, 32, 27, 27+, 10+, 6)
Time2<-c( 57, 51, 44, 32, 27, 27, 10, 6)
Status2<-c(0,0,0,1,1,0,0,1)
Surv(Time2,Status2)
survfit(Surv(Time2,Status2)~1)
philip2<-summary(survfit(Surv(Time2,Status2)~1))
philip2
n<-plot(survfit(Surv(Time2,Status2)~1),main="GROUP B",xlab='t',ylab='s(t)')

