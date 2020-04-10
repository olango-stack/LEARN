##regression analysis
setwd("C:/Users/user/Desktop/MULI")##setting the work directory
data<-read.csv("project.csv",header = T)##importing the data
data#display the imported data set
head(data)##gives a glimpse of the first six rows of the data
library(data.table)#load data.table package
library(dplyr)#load dplyr package
library(ggplot2)#load ggplot2 package
data1<-data.table(data)#convert into R data format
data1#display converted data
str(data1)#shows structure of data1
data2<-select(data1,-Travel_Route,-other_visitors)#leave out nuisance columns
data2#display required dataset

y1<-data2$total_visitors#assign value y to column
x1<-data2$Visitors_on_business#assign value x1 to column
x2<-data2$Visitors_on_employment#assign value x2 to column
x3<-data2$visitors_on_education#assign value x3 to column
##CHECKING THE DATA FOR REGRESSION ASSUMPTIONS
#a.background checks for multicollinearity 
#Pearson's correlation coefficient
round(cor(cbind(y1,x1,x2,x3)),2)#to 2 decimal places
#b.tolerance check
#refer to code line 49&50.tolerance check is done after fitting model


#GRAPHICAL ANALYSIS OF VARIABLES

##scatter plots for response variable against each  predictor variable  
par(mfrow=c(1,3))
scatter.smooth(x=x1,y=y1,main="total_visitors~investments",col='blue')
scatter.smooth(x=x2,y=y1,main="total_visitors~employment",col="purple")
scatter.smooth(x=x3,y=y1,main="total_visitors~education",col="red")
##boxplot checking for outliers within the variable data points
par(mfrow=c(1,4))
boxplot(y1,main="total_visitors",sub=paste("Outlier rows:",boxplot.stats(y1)$out))
boxplot(x1,main='investments',sub=paste('Outlier rows:',boxplot.stats(x1)$out))
boxplot(x2,main='employment',sub=paste('Outlier rows:',boxplot.stats(x2)$out))
boxplot(x3,main='education',sub=paste('Outlier rows:',boxplot.stats(x3)$out))



fit<-lm((y1~x1+x2+x3),data = data2)#fitting the model
fit##displays parameter coefficients
summary(fit)#gives a summary of model indicators
summary(fit)$coefficient#examine coeffients table
library(car)#load car package used in applied regression
vif(fit)##checking tolerance for predictor variables
##MODEL VARIABILITY-RESIDUAL ANALYSIS
#i)checking for normality
par(mfrow=c(1,2))#split plot panel to 1 by 2 grid
hist(resid(fit),main='Histogram of residuals',xlab='Standardised
Residuals',ylab='Frequency',col='black')##plots histogram of residuals
#ii)checking for homoscedasticity-same variance 
plot(fit,which = 1)#residual vs fitted plot


##fitting constructed model
fit1<-lm((y1~x1+x2),data = data2)#fitting best model for the data 
fit1#displays parameter coefficients
summary(fit1)#gives a summary of model indicators
summary(fit1)$coefficient #examine coeffients table
