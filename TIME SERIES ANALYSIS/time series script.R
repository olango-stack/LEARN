kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
kts<-ts(kings)
kts
#frequencies: yearly=1(usually the default),monthly=12,quarterly=4
#start=(2000,2):2nd quarter of that year
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
bts<-ts(births,frequency = 12)
bts
bts<-ts(births,frequency = 12,start=c(1946,1))#best option ,includes the years
bts
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenir
sts<-ts(souvenir,frequency = 12,start = c(1987,1))
sts
plot.ts(kts)#ts could b descrbd by additive model
#Random fluct roughly constant in size over time

plot.ts(bts)#-seasonal variations evident(peak and troughs) 
#-ts could b descr using additive model(seasonal&random fluct roughly constant over time)

plot.ts(sts)#seasonal & random flucts incr with level of timeseries
#additive model not appropriate
log.sts<-log(sts)#log transformation to enable application of additive model.
plot.ts(log.sts)#now size of seasonal &random flucts seems roughly const over time
#both do not depend on thr lev of time series

#DECOMPOSING a time series model
#means to separate ts into its constituent components
#a)Non-seasonal data(only trend+irregular comp )
#for trend;smoothing (by SMA method)
library(TTR)#libray for smoothing using SMA
kts1<-SMA(kts,n=3)#moving average of order 3
plot.ts(kts1,main="smoothed kts time series mov ave order 3")
kts2<-SMA(kts,n=8)#higher order due to presence of fluctuations when order 3
plot.ts(kts2,main="smoothed kts time series mov ave order 8")#trend becomes clearer
#b)Decomposing seasonal data
bts1<-decompose(bts)#estimating trend,seasonal&irregular components
bts1
bts1$seasonal#estimated values of the seasonal component
#evidently estimated seasonal factors same for each year
#Largest factor-1.46(July)reps peak,lowest-2.08(Feb)reps trough in births
plot(bts1)#plot estimated trend,seasonal & irregular components
#estimated trend drops 24 to 22(1947,1948),then steady increase onwards(upto 27(1959))

#seasonal adjusting (for seasonal time series that can be fit by additive model)
#done by subtracting estimated seasonal comp from original time series
#decompose()-used 2 estimate seasonal components(we get estimate of seasonal component)
bts2<-bts-bts1$seasonal#birth time series(bts) seasonally adjusted
plot(bts2)#Plot seasonally adjusted time series
#the plot lacks seasonal components(has trend & random though)

##EXPONENTIAL SMOOTHING

#exponential smoothing-make short term forecasts for time series data

#*Simple exponential smoothing-usd with timeseries that can b descr with additive model;

#with constant level& without seasonality
#smoothing controlled by parameter alpha(ranges from 0 to 1)
#smoothing estimates level at current time point
#alpha close to 0-little weight put on most recent obs wen makin forecasts
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rain
rainseries <- ts(rain,start=c(1813))
rainseries
plot.ts(rainseries)#time series plot for the data.constant mean(level) visible-about 25
#random flucts seem constant in size;so additive model appropriate 
#this gives greenlight for forecast using simple exponential smoothing
#HoltWinters()-fits a simple exponential predictive model
#To use HoltWinters() fxn,set beta and gamma =FALSE;coz this is simple expo smoothing
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts
#alpha 0.02 ,close to 0.implies more weight placed on obs from the more distant past
#corresponding output (rainseriesfore..) is a list with variables
rainseriesforecasts$fitted#variable 'fitted' exists in list above.fitted contains 4casts
plot(rainseriesforecasts)#plots original vs fitted(smoothed)
#now to measure accuracy of forecasts;we calc SSE(another var in forecast list)
rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)#l.start (first level to include)
#23.56 here is rainfall value for 1813.code serves same fxn as code #71
#note that HoltWinters() makes forecasts for period covered by original data
#forecast.HoltWinters() fxn(in forecast pack)-forecast outside data(extrapolation)
library(forecast)
#forecast.HoltWinters() suitable for use  after HoltWinters()
rainseriesforecasts2<-forecast(rainseriesforecasts, h=8)#forecast fxn served purpose
rainseriesforecasts2#shows 80&95% prediction intervals for the forecast
plot(rainseriesforecasts2)#plots predictions
#1913-1920 predictions(forecasts) plotted as blue line
#dark grey area-80% prediction interval
#light grey area-95% prediction interval
rainseriesforecasts2$residuals#forecast errors 
Acf(rainseriesforecasts2$residuals, lag.max=20)#correlogram for the errors
#autocor at lag 3 touches signif bounds
#no correlations between forecast errors if predictive model cant b improved
#if forecast errors r related ,then SESmoothing forecasts can be imprvd(by other tekniks
#to test for non-zero autocorrelation ,use Ljung-box test
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#L-Box testatistic =17.4,pvalue=.6 i.e little evidence of nonzero autocorr is sample forecast errors
#another way to check whether pred model cant be imprvd is;
#check whether forecst errors are norm dist with mean 0 & constant var
#thus to check for constant var we make a trad ts plot
plot.ts(rainseriesforecasts2$residuals)#confirmed non constant var
#now to check for 0 mean(norm dist),plot histogram of forecast errors
plotForecastErrors <- function(rainseriesforecasts2$residuals)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(rainseriesforecasts2$residuals)/4
  mysd <- sd(rainseriesforecasts2$residuals)
  mymin <- min(rainseriesforecasts2$residuals) - mysd*5
  mymax <- max(rainseriesforecasts2$residuals) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin)
    { mymin <- mymin2 }
  if (mymax2 > mymax) 
    { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed
  
    mybins <- seq(mymin, mymax, mybinsize)
  hist(rainseriesforecasts2$residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast

    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(rainseriesforecasts2$residuals)#plot histogram using defined fxn
#L-Box test showed no autocorr,then also  dist of forecast errors~N(0,const var)
#i.e simple expo smoo provides adequate predictive model for the data

##Holts exponential smoothing

#good for;ts descri by additive model;trend(incr/decr);no seasonality
#used to make short term forecasts
#estimate level & slope at current time point
#two parameters;alpha(est of level at current time point)&beta(est of slope(b) of trend comp at current time)
#both parameters lie (0,1).value close to 0-little weight on mst recent obs wen makin 4cast
      

#eg time series of annual diameter of women's skirts(1866-1911)


skirts <- scan("https://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
#plot shows increase from 600 to 1050 then decrease to 520
#we fit predictive model using HoltWinters() fxn
#for Holt's expo smooth we set gamma=F(gamma is used for Holt Winters expo smoothing)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE#SSE for the forecast errors
#both alpha and beta high ie estimate of current lev &slope of trend r based on most recent obs
plot(skirtsseriesforecasts)#plot of the in-sample forecasted values
#from plot we can see in-sample forecasts agree pretty well with observed values(they lag behind a little tho)

#you can specify initial lev value & slope(b) of trend component(if you like)
# use l.start & b.start
#l.start usually first val in time series(ie 608 here)
#b.start usually 2nd -1st val(ie 617-608=9 here)
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)#compare to code #152,same thing

#now let's make forecast for points outside data points



skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
skirtsseriesforecasts2
plot(skirtsseriesforecasts2)
#forecasts shown in blue line
#80% pred intervals as deep grey & 95% pred interv as light grey area

##Now we check if pred model can be improved or not

#first we chek if the in-sample 4cast errors show nonzero autocorr(yes corr)btwn, say, lag 1-20
#so we plot ACF and do Ljung-Box test
Acf(skirtsseriesforecasts2$residuals, lag.max=20)
#sample autocorr for e insample forecast err at lag 5 exceeds the sig bounds
#we xpect 1 in 20 of the autocorrs 4 1st 20 lags to exceed 95% sig bound by chance(ovias)

Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#p value=0.47 ie little evidence of nonzero autocorrelations at lags 1-20
#19.731 is the Ljung box test statistic

##2nd we check for constant var in forecst err & if forecasterr~N(0, const var)
plot.ts(skirtsseriesforecasts2$residuals)#checks for constant var
#now to check if forecasterrors~N(0 mean)

##define an R function “plotForecastErrors()”

plotForecastErrors <- function(skirtsseriesforecasts2$residuals)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(skirtsseriesforecasts2$residuals, na.rm = TRUE)/4
  mysd   <- sd(skirtsseriesforecasts2$residuals, na.rm = TRUE)
  mymin  <- min(skirtsseriesforecasts2$residuals, na.rm = TRUE) - mysd*5
  mymax  <- max(skirtsseriesforecasts2$residuals, na.rm = TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm, na.rm = TRUE)
  mymax2 <- max(mynorm, na.rm = TRUE)
  if (mymin2 < mymin ) { mymin <- mymin2}
  if (mymax2 > mymax) { mymax <- mymax2}
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(skirtsseriesforecasts2$residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#the function encapsulates what it needs for the plot &creates fxn plotForecastErrors


#plot a histogram (with overlaid normal curve) of the forecast errors for the rainfall predictions
plotForecastErrors(skirtsseriesforecasts2$residuals
                   
#histogram above shows it's plausible that forecast err have 0 mean & const var
#In conclusion ,Holt's expo smoothing provides adequate predictive model for this data
#This predictive model cant be improvd upon (as proved by tests on forecast errors)
#this means 80% & 95% prediction interv are valid

####Holt-Winters Exponential Smoothing####

#for ts descr by additive model; with decr/incr trend;seasonality present
#make short term forecasts
#estimates the level,slope(of trend)&seasonal component at current timepoint.
#Smoothing controlled by 3 parameters(alpha,beta & gamma)
#alpha:est level at current time point
#beta:est slope(b) of trend comp at current time point
#gamma:est seasonal component at current time point
#values clos to 0-little weight put on most recent obs wen 4casting
#an example is the souvenir monthly sales data

souvenirtimeseriesforecasts <- HoltWinters(log.sts)#from code #25
souvenirtimeseriesforecasts
souvenirtimeseriesforecasts$SSE
#alpha=0.41,rel low;est of lev at curr time point based upon recent vals & more dist vals
#beta=0;slope b of trend comp remains roughly the same
#gamma=0.96,high;est of seasonal comp based upon recent values

plot(souvenirtimeseriesforecasts)
#gives polt of observed vs fitted
#we can see predicted seasonal peaks ,occur roughly in NOV

#Now lets make predictions outside data points;
souvenirtimeseriesforecasts2<-forecast(souvenirtimeseriesforecasts,h=48)#pred 4 4yrs
souvenirtimeseriesforecasts2
plot(souvenirtimeseriesforecasts2)#dark grey area(80% pred interv),lightgrey(95% pred int)
#Now we check if predictive model can be improved
#check if forecast err show nonzero autocorr(acf)&do Ljung-Box test;
Acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
#the autocorr for the forecast errors dont exceed signif bounds;
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#test stat =17.53,pvalue=0.6 

#code #257& #259 indicate little evidence of nonzero autocorrelations at lag 1-20

#DETOUR TO UNDERSTAND AUTOCORRELATION CONCEPT

#NOTE:Randomness is ascertained by computing autocorrelations
#autocorr near zero if random & one /more autocorr significantly non-zero if non-random
#each spike(vert line) below/abve dotted line;is stat significant
#this means spike has value sign dffnt from zero ie evidence of autocorrelation
#spike close to zero is evidence against autocorr i.e no autocorr

#Now checking forecasterr for nonconstant var & norm dist with 0 mean
plot.ts(souvenirtimeseriesforecasts2$residuals)#check for const var
#we see that forecast errors have constant variance over time

plotForecastErrors <- function(souvenirtimeseriesforecasts2$residuals)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(souvenirtimeseriesforecasts2$residuals, na.rm = TRUE)/4
  mysd   <- sd(souvenirtimeseriesforecasts2$residuals, na.rm = TRUE)
  mymin  <- min(souvenirtimeseriesforecasts2$residuals, na.rm = TRUE) - mysd*5
  mymax  <- max(souvenirtimeseriesforecasts2$residuals, na.rm = TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm, na.rm = TRUE)
  mymax2 <- max(mynorm, na.rm = TRUE)
  if (mymin2 < mymin ) { mymin <- mymin2}
  if (mymax2 > mymax) { mymax <- mymax2}
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(souvenirtimeseriesforecasts2$residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
#the function encapsulates what it needs for the plot &creates fxn plotForecastErrors


#plot a histogram (with overlaid normal curve)

plotForecastErrors(souvenirtimeseriesforecasts2$residuals)
#forecast errors are normally distributed with mean zero

##CONCLUSIONS
#From the time plot,hist& acf  there is little evidence of autocorr btwn forecasterrors
#also the forecast errors are normally dist with zero mean &constant var over time
#Holt-Winters expo smooth provides adequate pred model for the data
#pred model cant be improved
#assumptions upon which prediction intervals are based r VALID.

####ARIMA MODELS####

#takes correlation(between successive values ) into account 
#includes a stat model for irregular component of a time series;
#the model allows for nonzero autocorr in the irregular component
#ARIMA models are for stationary time series
#if you have nonstationary time series ,difference to make it stationary
#if you have to diff d times to get a stationary ts,then you have an ARIMA(p,d,q)
skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)
skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)
install.packages("fUnitRoots")# Perform formal stationarity tests ie unit root tests
#time series appears stationary after second difference(const var &mean)
#this shows we can use ARIMA(p,2,q) model for the time series
ktsdiff1<-diff(kts,differences = 1)#the original kts wasn't stationary in mean
plot.ts(ktsdiff1)
#the t.series now appears to be stationary in mean & var after differene
#an ARIMA(p,1,q)
#by taking the difference we remove trend ,we're left with irregular comp
#now we examine for autocorrelation in the irregular component
#this helps build a predictive model for the data

#selecting a candidate ARIMA model
#make this step after ensuring ts is stationary ,if not difference first
#basically involves gettin values of p&q for ARIMA(p,d,q) model
#to get p & q we use ACF & PACF,setting plot=FALSE

  #FOR ACF;                         #FOR PACF
#AR(p) -Tails off gradually         cuts off after p lags
#MA(q)-cuts off after lag q         tails off gradually
#ARMA(p,q)-tails off gradually      tails off gradually
acf(ktsdiff1,lag.max = 20)#acf plot (correlogram)
acf(ktsdiff1,lag.max = 20,plot = F)#just displays values of autocorr values
#its noticeable spike at lag1 exceeds significance bound,the rest dont

#Now for PACF
pacf(ktsdiff1,lag.max = 20)#pacf plot(partial correlogram)
pacf(ktsdiff1,lag.max = 20,plot = F)#display just the partial autocorr values
#the partial autocorrelations tail off to zero after lag 3

#now since ACF is zero after lag 1,PACF tails off to zero after lag 3
#meaning;ARMA (3,O)
        #;ARMA(0,1)   are possible for the time series of first difference
        #;ARMA(p,q) with p&q >0(mixed model)
#model with the fewest parameters is best(parsimony) ie we take ARMA(0,1)/MA(1)
#MA Usd to model ts with short term dependencies btwn successive obsv
#therefore the data can be modelled using ARIMA(0,1,1)
#OR
library(forecast)
auto.arima(kts)#shortcut for getting the correct model

#ARIMA example2
#volcanic dust veil index (1500-1969)
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
#appears random fluctuations are roughly constant;additive model approp
#appears to be stationary in mean and var
#Now plot acf and pacf to decide on which model to use

acf(volcanodustseries, lag.max=20)#acf plot(correlogram)
acf(volcanodustseries, lag.max=20, plot=FALSE)#autocorrelation values
#correlations tail off to zero after lag 3
#corr for lag 19 & 20 exceed sig bounds(could be due to chance)
pacf(volcanodustseries, lag.max=20)#pacf plot
pacf(volcanodustseries,lag.max = 20,plot = F)#partial autocorr values
#the partial autocorr tail off to zero after lag 2
#possible models;
                # ARMA(2,0),ARMA(0,3)& ARMA(p,q)
#auto arima
auto.arima(volcanodustseries)
auto.arima(volcanodust)
#these two codes yield same results
auto.arima(volcanodust,ic="bic")#uses bic criterion(penalizes the no of parameters)
#gives ARIMA(2,0,0)-kinda coincides with the acf &pacf preferable model by parsimony
#An ARMA(2,0) = AR(2)
#AR usually used to model ts showing longer dependencies btwn succ. observ
#An ARIMA(2,0,0) can be used

#Forecasting using ARIMA
#After selecting best cand model,estimate it's parameters,then it for forecasting
ktsarima<-arima(kts,order = c(0,1,1))
ktsarima
#estimated value of theta =-0.7218
#specify the pred intervals ie level=c(95)
ktsforecasts <- forecast(ktsarima, h=5)
ktsforecasts#age of death for the next 5 kings
plot(ktsforecasts)

#investigate if forecasterrors ~N(0,const) &also if corre exist btwn succ errors
acf(ktsforecasts$residuals,lag.max = 20)
Box.test(ktsforecasts$residuals,lag = 20,type = "Ljung-Box")
#no sample autocorr exceeds sig bounds,pvalu=.9 ie little evidence for nonzero autocor
plot.ts(ktsforecasts$residuals)
#var is roughly constant over time

plotForecastErrors <- function(ktsforecasts$residuals)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(ktsforecasts$residuals)/4
  mysd <- sd(ktsforecasts$residuals)
  mymin <- min(ktsforecasts$residuals) - mysd*5
  mymax <- max(ktsforecasts$residuals) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin)
  { mymin <- mymin2 }
  if (mymax2 > mymax) 
  { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed
  
  mybins <- seq(mymin, mymax, mybinsize)
  hist(ktsforecasts$residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast
  
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(ktsforecasts$residuals)
#forecast errors roughly norma dist,mean close to zero
#conclusion;ARIMA(0,1,1) good pred model for the data

#example the volcano dust index ex
#ARIMA(2,0,0) was found approp
#lets fit an ARIMA(2,0,0) to the series

volcanodustseriesarima<-arima(volcanodustseries, order =c(2,0,0))
volcanodustseriesarima
#parameters beta1 &beta2 are est to be 0.7533 and -0.1268
#after fitting we can use to forecast 
volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
volcanodustseriesforecasts
#notice the negative values in the forecasts
#the arima() & forecast() don't know var only takes + values
plot(volcanodustseriesforecasts)
#once forecast has been done ,test forecast errors
acf(volcanodustseriesforecasts$residuals, lag.max=20)
Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
#sample autocorr at lag 20 exceeds the bounds(explained by totally due to chance)
#p value says there is little evidence for nonzero autocorr for lags 1-20

#now check for normality(0 mean ) and constant var
plot.ts(volcanodustseriesforecasts$residuals)
#time plot indicates roughly constant variance

plotForecastErrors <- function(volcanodustseriesforecasts$residuals)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(volcanodustseriesforecasts$residuals)/4
  mysd <- sd(volcanodustseriesforecasts$residuals)
  mymin <- min(volcanodustseriesforecasts$residuals) - mysd*5
  mymax <- max(volcanodustseriesforecasts$residuals) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin)
  { mymin <- mymin2 }
  if (mymax2 > mymax) 
  { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed
  
  mybins <- seq(mymin, mymax, mybinsize)
  hist(volcanodustseriesforecasts$residuals, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast
  
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(volcanodustseriesforecasts$residuals)
#hist plot indicates negative mean instead of zero mean;dist is skewed(right)
#proof
mean(volcanodustseriesforecasts$residuals)
#we cant conclude that forecasterr~N(0,const)
#This implies that the ARIMA(2,0,0) isnt the best and can be improved on

