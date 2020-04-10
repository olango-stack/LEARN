##this is some code on common plots
setwd("C:/Users/user/Desktop/R DIRECTORY")
##ggplot2 package
library(ggplot2)
library(gridExtra)
##gridExtra package makes ggplot2 more powerful
##eg comparing multiple plots in one window
data("ToothGrowth")#toolgrowth in guinea pigs
df<-ToothGrowth
df$dose<-as.factor(df$dose)
df$dose
head(df)
#BOXPLOT
bp<-ggplot(df,aes(x=dose,y=len,color=dose))+geom_boxplot()+theme(legend.position = "none")
bp##code above has grid lines
#adding grid lines
bp+background_grid(major="xy",minor="none")##doesnt run
##SCATTER PLOT
sp<-ggplot(mpg,aes(x=cty,y=hwy,color=factor(cyl)))+geom_point(size=2.5)
sp
#BARPLOT
barplot<-ggplot(diamonds,aes(clarity,fill=cut))+geom_bar()+theme(axis.text.x = element_text(angle = 70,vjust = 0.5))
barplot
##comparing two plots
library(cowplot)
plot_grid(sp,barplot,labels=c("A","B"),ncol=2,nrow=1)#package cowplot is essential for this code
###HISTOGRAMS
ggplot(diamonds,aes(x=carat))+geom_histogram(binwidth = 0.25,fill="purple")+scale_x_continuous(breaks = seq(0,3,by=0.5))

