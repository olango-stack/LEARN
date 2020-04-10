setwd("C:/Users/user/Desktop/R DIRECTORY")
#data.table package
#allows faster manipulation in data set,minimum coding
#reduced computing time comp to data.frame
#now to load data 
data("airquality")#data for air quality in New York MAY-SEP 1973
mydata<-airquality
head(airquality,6)
#load data
data(iris)
myiris<-iris
##load data.table package
library(data.table)
mydata<-data.table(mydata)
mydata
myiris<-data.table(myiris)
myiris
##select/subset 2nd to 4th row -weather data
mydata[2:4,]
#select col with particular value-iris data
myiris[Species=="setosa"]#in this case pick out 'setosas' only
#select cols with multiple values
#eg select cols with setosa &virginica
myiris[Species %in% c("setosa","virginica")]
#select columns.Returns vectors
mydata[,Temp]
mydata[,.(Temp,Month)]#display temp and month columns
mydata[,c(Temp,Month)]#joint vector for temp and month columns
#getting sum of a selected column
mydata[,sum(Ozone,na.rm=TRUE)]
#GETTING BOTH SUM AND STD DEV
mydata[,.(sum(Ozone,na.rm=TRUE),sd(Ozone,na.rm=TRUE))]
#Now to print and plot
myiris[,{print(Sepal.Length)
  plot(Sepal.Width)
  NULL}]
#GROUPING BY VARIABLE
myiris[,.(sepalsum = sum(Sepal.Length)), by=Species]#totals za sepallength by species
#Select col for computation
#hence need to set key on column
setkey(myiris,Species)#memory fxn ,R will act based on this command

myiris["setosa"]
myiris[c("setosa","virginica")]
##Importing from url/web
library(data.table)
mydat<-fread('C://Some/File/Path.csv')#could be used to do the normal importing 
mydat1<-fread('https://redirect.viglink.com/?format=go&jsonp=vglnk_158571113844813&key=949efb41171ac6ec1bf7f206d57e90b8&libId=k8gqre2y01021u9s000DLbglb6osk&loc=https%3A%2F%2Fwww.r-bloggers.com%2Fgetting-data-from-an-online-source%2F&v=1&type=U&out=http%3A%2F%2Fwww.stats.ox.ac.uk%2Fpub%2Fdatasets%2Fcsb%2Fch11b.dat&ref=https%3A%2F%2Fwww.google.com%2F&title=Getting%20Data%20From%20An%20Online%20Source%20%7C%20R-bloggers&txt=http%3A%2F%2Fwww.stats.ox.ac.uk%2Fpub%2Fdatasets%2Fcsb%2Fch11b.dat')
#importing web data
head(mydat1)#gives a glimpse
mydata2<-fread('url.dat')
mydata2
library(RCurl)
myfile <- getURL('https://miro.medium.com/max/1400/0*b3poZbwBaw4Iy_io.jpg.csv',ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
myfile##returns unable to process image
class(myfile)
mydata3<-read.csv(textConnection(myfile),header = T)
mydata3
