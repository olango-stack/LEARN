setwd("C:/Users/user/Desktop/R DIRECTORY")
library(survival)
data("ovarian")
#CREATING SURVIVAL OBJECT
s1<-Surv(ovarian$futime,ovarian$fustat)
s1
#CREATE SURVIVAL CURVE,USING FORMULA eg Meier
fit1<-survfit(Surv(ovarian$futime,ovarian$fustat)~1,data = s1)
fit1
#gets summary of S(t)+std error+CI
summary(fit1)
#plot survival funtion 
plot(fit1,xlab = "t",ylab=expression(hat(S)*"(t)"))
#fitting a parametric regression model assuming time to event follows expo dist
s2<-survreg(Surv(ovarian$futime,ovarian$fustat)~1,dist = "exponential", data = s1)
s2
summary(s2)
#here lambda=exp(-(intercept)) i.e =exp(-7.169)
#and S(t)=exp(-lambda(t)) i.e S(t)=exp(-exp(-7.17)t)
plot(T,1-pexp(T,exp(-7.169)),xlab = "t",ylab = expression(hat(S)*"(t)")
#fitting model assuming time to event follows weibull dist
s3<-survreg(Surv(ovarian$futime,ovarian$fustat)~1,data=ovarian ,dist ="weibull",scale = 0)
s3
#where S(t)=exp(-exp[-7.111/0.902])t^1/0.902
#and y=1/scale(identify from output),alpha=exp(-(intercept)y)
#weibull dist has parameters y and alpha in this case

