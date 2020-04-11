setwd("C:/Users/user/Desktop/R DIRECTORY")
5^5
5^2
require(datasets)##load and attach packages
attach(mtcars)##accessing database
library(ggplot2)
mtcars
counts<-table(vs,gear)#more of contigency table
counts
barplot(counts,main='car distribution')
barplot(counts,main = 'car distribution',horiz = T)
barplot(counts,main = 'car distribution',horiz=T,names.arg = c('3 gears','4 gears','5 gears'))
barplot(counts,xlab = 'x',ylab = 'y',legend=rownames(counts),beside = T,col=c('pink','purple'))
pie(counts,xlab='x',ylab='y',legend=rownames(counts),beside=T,col = c('yellow','red'))
x<-c(20,30,40,50)
names(x)<-c('pussy','money','weed',"bitchez")
pie(x,labels = names(x),radius = 1,col = c(1:4),main = "life")
plot(x,type = 'b',lty='dotted')##type b =both points and lines
x=1:9
y<-matrix(data=x,nrow=3,ncol=3,byrow = F)
y
##for loops
p<-matrix(0,3,3)#3by3 matrix of zeros
for(i in 1:9)
{p[i]<-i
}
p
##random variables
q<-matrix(0,3,3)
x<-rnorm(9)
x
length(x)
for(i in 1:length(x))
{
  q[i]<-x[i]
}
q
 #setseed random number generation
set.seed(4)
y<-rnorm(10)
y
p<-rnorm(10)
p
##switch
y<-c(2,3,5)
central<-function(y,measure){
  switch(measure,  
  Mean=mean(y),
  Geometric=exp(mean(log(y))),
  Harmonic=1/mean(1/y),
  Median=median(y),
  Arithmetic.Mean=mean(y),
  stop('Measure not included')}
central(y,'Geometric')
central(y,'Harmonic')
central(y,'Median')
central(y,'Arithmetic.mean')
##few plots
x<-c(1,2,3,4,5)
y<-c(1,2,6,10,20)
plot(x,y,type = 'l',col='purple')  
grid(nx=NULL,lty = "dotted",col = 'purple')
plot(x,y,main='my plot',type = 'l',col="green",sub = 'figl')
grid(nx=NULL,lty = 'dotted',col = 'green')
plot(x,y,main = "my plot",type = 'b',col='20',pch=15)#pch changes shapes of point plots
x<-c(2,2,2,3,3,4)
c<-table(x)
c
barplot(x,main='bangi',col = '10',sub='shash')
barplot(c,horiz = T)
barplot(c,col = 'yellow',names.arg = c('portmore',';gaza','genje'),main = 'mbogi')
pie(c,col = (3:6),labels = c('Pac','Biggie','Jay'))
x<-c(1,2,3,4,5)
y<-c(1,2,6,10,20)
p<-c(1.5,2.5,6.2,8,10)
plot(x,y,type='l')
lines(p)#adds line to existing plot
abline(h=2)#adds one or two lines to current plot-horizontally
abline(v=2)#vertical line
text(3,4,"v.good",col='red')#adds comment at the coordinates
legend(4,16,c('sine','cos'),col=c('blue','yellow'))
abline(1,1,col='pink')
install.packages('LAPACK')##PACKAGE FOR solving systems of linear equations and linear least squares, eigenvalue problems, and singular value decomposition
##the bioconductor repository
source("https://bioconductor.org/biocLite.R")#get basic functions
library(BiocManager)#this is what works for version 3.5 or >
installed.packages()#checking installed packages
library()
old.packages()#check packages that need an update
update.packages()
detach("package:ggplot2", unload=TRUE)#unloading package
sessionInfo()#version of R plus list of loaded packages.can run code in console
browseVignettes("ggplot2")##more of guidelines
install.packages('vioplot')#violin plot
install.packages("RDocumentation")
library(RDocumentation)
help(package = "vioplot")
install.packages('KernSmooth')
library(KernSmooth)


#solving fxns in R
#say 5x=10
solve(5,10)#gives x=2
##3x+2y=8 & x+y=2
a<-matrix(c(3,1,2,1),2,2)
a
b<-matrix(c(8,2),2,1)
b
solve(a,b)
#LOOPS AND CONDITION STATEMENTS

##FOR LOOPS
samples<-c(rep(1:10))
samples
for (thissample in samples)

{
 print(thissample) 
}
#now sth inside the for loop
for (thissample in samples)
{
  str<-paste(thissample,'is current sample',sep='')
  print(str)
}
#terminate the loop when the sample =3
for (thissample in samples)
{
  if (thissample==3)break
  str<-paste(thissample,'is current sample',sep='')
  print(str)
}  
#ignore when the sample number is even
for (thissample in samples)
{
  if(thissample %%2 ==0)next
  str<-paste(thissample,'is current sample',sep='')
  print(str)
}  
##the last three samples
end<-length(samples)
begin<-end - 2
for(thissample in begin:end)
{
  str<-paste(thissample,'is current sample',sep='')
  print(str)
}
#the break statement is used to terminate the loop abruptly
#the next statement is used to just ignore current cycle

##IF ELSE STATEMENT
#Syntax if (condition) {...}else{...}.if else statement can be nested
samples<-c(rep(1:10))
samples
#even sample numbers using if else
for(thissample in samples)
{
  if (thissample %% 2 !=0)next
  else print(thissample)
}  
#the ifelse fxn is a vectorized version of if else.
#it's syntax is ifelse(condition,v1,v2)
#ie if condition is true return v1 ,otherwise v2
#eg if we want samples with no. >6 be number 2,and those not be 1s
ret<-ifelse(samples>6,2,1)
ret

##repeat

#similaar to while and for 
#execute block of commands repeatedly till break
total<-0
repeat{total<-total+1;print(total);if (total>6) break;}

##while loop
#executes a block of commands until cond is no longer satisfied
#syntax is while(cond)expr
x<-1
while(x<5){x<-x+1;print(x);}
#next can skip one of the loop,break will end loop abruptly

#eg break when x=3
x<-1
while(x<5){x<-x+1;if(x == 3 )break;print(x); }
#eg skip when x=3
x<-1
while(x<5){x<-x+1;if (x==3)next;print(x) }

##which function

#-gives true indices of a logical object
#-allows for array indices this way

#syntax      which(x, arr.ind = FALSE, useNames = TRUE)
#            arrayInd(ind, .dim, .dimnames = NULL, useNames = FALSE)

#x :logical array/vector.NAs are allowed and omitted (treated as if FALSE)
#arr.ind :logical;should array indices be returned when x is an array?
#ind:integer valued index vector,as resulting from which(x)
#.dim:integer vector
#.dimnames:optional list of character dimnames(.), of which only .dimnames[[1]] is used
#useNames:logical indicating if the value of arrayInd() shld have (non-null) dimnames at al
which (letters == "h" )
BOD #Biochemical oxygen demand data frame
which(BOD$demand == 16)
x <- matrix(1:9,3,3)
x
which(x %% 3 == 0, arr.ind=TRUE)
which(x %% 3 == 0, arr.ind=FALSE)
