setwd("C:/Users/user/Desktop/R DIRECTORY")
##reshape2 package
#useful in reshaping data.uses melt and cast functions
#melt-converts data from wide format to long format
#multiple categorical columns melted into unique rows
#NOW,
#create data
ID<-c(1,2,3,4,5)
Names<-c("Joseph","Matrin","Joseph","James","Matrin")
DateofBirth<-c(1993,1992,1993,1994,1992)
Subject<-c("Maths","Biology","Science","Psycology","Physics")
thisdata<-data.frame(ID,Names,DateofBirth,Subject)
thisdata
library(data.table)
data.table(thisdata)
library(reshape2)
##melt
mt<-melt(thisdata,id=(c("ID","Names")))
mt
##cast
#converts data from long format to wide format
#converts melted data to normal using 2 fxns dcast and acast
mcast<-dcast(mt,DateofBirth+Subject~variable)
mcast
#dcast returns dataframe as output
#acast returns a vector/matrix/array as output