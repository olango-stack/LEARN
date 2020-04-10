setwd("C:/Users/user/Desktop/R DIRECTORY")
##the tidyr package
#great along with dplyr
#uses key functions gather,spread,separate,unite
library(tidyr)
names <- c('A','B','C','D','E','A','B')
weight <- c(55,49,76,71,65,44,34)
age <- c(21,20,25,29,33,32,38)
Class <- c('Maths','Science','Social','Physics','Biology','Economics','Accounts')
#create data frame
tdata<-data.frame(names,age,weight,Class)
tdata
#gather function-convert from wide to long format.same as melt in reshape2 pack
long_t<-tdata%>% gather(Key,Value,weight:class)
long_t
#separate fxn-splits col into multiple columns
#best use when provided time variable in the dataset
Humidity <- c(37.79, 42.34, 52.16, 44.57, 43.83, 44.59)
Rain <- c(0.971360441, 1.10969716, 1.064475853, 0.953183435, 0.98878849, 0.939676146)
Time <- c("27/01/2015 15:44","23/02/2015 23:24", "31/03/2015 19:15", "20/01/2015 20:52", "23/02/2015 07:46", "31/01/2015 01:55")
#now creating a data frame
d_set<-data.frame(Humidity,Rain,Time)
d_set
separate_d<-d_set%>%separate(Time,c('Date','Month','Year'))
separate_d
#unite-unites multiple columns into single column
unite_d<-separate_d%>%unite(Time,c(Date,Month,Year),sep = "/")
unite_d
#the spread fxn
#takes a key:value pair and converts it into separate columns
wide_t<-long_t%>%spread(Key,Value)
wide_t
