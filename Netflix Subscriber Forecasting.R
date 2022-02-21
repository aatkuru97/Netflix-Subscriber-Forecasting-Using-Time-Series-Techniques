#Time Series Project : Netflix Subscribers Forecasting


#Average, Naive, Seasonal Naive, and Drift Methods
library(fpp2)
library(fpp)
library(urca)

subscribers_trainingdata<-ts(c(1.39,1.9,-1.8,0.1,2.83,1.28,1.78,2.87,3.88,1.4,2.37,3.42,4.7, 1.86, 2.66,3.83,5.14,2.46,3.94,4.82,6.87,2.19,3.38,5.81,5.27,4.68,4.98,6.62), start = 2011, freq = 4)
subscribers_testingdata<-ts(c(1.39,1.9,-1.8,0.1,2.83,1.28,1.78,2.87,3.88,1.4,2.37,3.42,4.7, 1.86, 2.66,3.83,5.14,2.46,3.94,4.82,6.87,2.19,3.38,5.81,5.27,4.68,4.98,6.62,8.26,5.45,6.07,8.84,9.6,2.7,6.77 ), start = 2011, freq = 4)

#subscribers is up to Q4 2017
#NAIVE
naivemethod = naive(subscribers_trainingdata,7)
naivemethod
plot(naivemethod, main = "Additions in Subscriber Forecast: Naive Method", ylab = "Number of Subscribers (Millions)", xlab = "Year")
accuracy(naivemethod,subscribers_testingdata)
#SIMPLE AVG
simpleaverage = meanf(subscribers_trainingdata,7)
simpleaverage
plot(simpleaverage, main = "Additions in Subscriber Forecast: Average Method", ylab = "Number of Subscribers (Millions)", xlab = "Year")
accuracy(simpleaverage,subscribers_testingdata)
#SEASONAL NAIVE
seasonalnaivemethod = snaive(subscribers_trainingdata, 7, freq=4)
seasonalnaivemethod
accuracy(seasonalnaivemethod,subscribers_testingdata)
plot(seasonalnaivemethod, main = "Additions in Subscriber Forecast: Seasonal Naive", ylab = "Number of Subscribers (Millions)", xlab = "Year" )
#DRIFT
driftmethod = rwf(subscribers_trainingdata, 7, drift = TRUE)
driftmethod
plot(driftmethod, main = "Additions in Subscriber Forecast: Drift Method", ylab = "Number of Subscribers (Millions)", xlab = "Year" )
accuracy(driftmethod,subscribers_testingdata)

library(fpp2)
library(fpp)
library(urca)

subscribers_testingdata<-ts(c(1.39,1.9,-1.8,0.1,2.83,1.28,1.78,2.87,3.88,1.4,2.37,3.42,4.7, 1.86, 2.66,3.83,5.14,2.46,3.94,4.82,6.87,2.19,3.38,5.81,5.27,4.68,4.98,6.62,8.26,5.45,6.07,8.84,9.6,2.7,6.77 ), start = 2011, freq = 4)
subscribers_trainingdata<-ts(c(1.39,1.9,-1.8,0.1,2.83,1.28,1.78,2.87,3.88,1.4,2.37,3.42,4.7, 1.86, 2.66,3.83,5.14,2.46,3.94,4.82,6.87,2.19,3.38,5.81,5.27,4.68,4.98,6.62), start = 2011, freq = 4)

#subscribers is up to Q4 2017
#NAIVE
naivemethod = naive(subscribers_testingdata,7)
naivemethod
plot(naivemethod, main = "Additions in Subscriber Forecast: Naive Method", ylab = "Number of Subscribers (Millions)", xlab = "Year")
accuracy(naivemethod,subscribers_testingdata)
#SIMPLE AVG
simpleaverage = meanf(subscribers_testingdata,7)
simpleaverage
plot(simpleaverage, main = "Additions in Subscriber Forecast: Average Method", ylab = "Number of Subscribers (Millions)", xlab = "Year")
accuracy(simpleaverage,subscribers_testingdata)
#SEASONAL NAIVE
seasonalnaivemethod = snaive(subscribers_testingdata, 7, freq=4)
seasonalnaivemethod
accuracy(seasonalnaivemethod,subscribers_testingdata)
plot(seasonalnaivemethod, main = "Additions in Subscriber Forecast: Seasonal Naive", ylab = "Number of Subscribers (Millions)", xlab = "Year" )
#DRIFT
driftmethod = rwf(subscribers_testingdata, 7, drift = TRUE)
driftmethod
plot(driftmethod, main = "Additions in Subscriber Forecast: Drift Method", ylab = "Number of Subscribers (Millions)", xlab = "Year" )
accuracy(driftmethod,subscribers_testingdata)


#Moving Average
subscribers<-c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28, 1.78, 2.87,
               3.88, 1.4, 2.37, 3.42, 4.7, 1.86, 2.66, 3.83, 
               5.14, 2.46, 3.94, 4.82, 6.87, 2.19, 3.38, 5.81,
               5.27, 4.68, 4.98, 6.62, 8.26, 5.45, 6.07, 8.84, 
               9.6, 2.7, 6.77)
subscribers
ma3 <- ma(subscribers, 3)
ma3
ma5 <- ma(subscribers, 5)
ma5

subscribers= ts(subscribers, start = 2011, frequency = 4)

plot(subscribers, main="Additional Netflix subscribers worldwide", 
     type = 'l', ylab="Subscribers", xlab="Years")
abline(h=0, col = "black", lty = 2)
lines(ma(subscribers,3),col="blue")
legend("topleft",legend=c("3-MA"),col=c("blue"),bty="n",cex=1.0,horiz=F,lwd=1)
lines(ma(subscribers,5),col="red")
legend("topleft",legend=c("3-MA", "5-MA"),col=c("blue", "red"),bty="n",cex=1.0,horiz=F,lwd=1)
lines(ma(subscribers,7),col = "green")
legend("topleft",legend=c("3-MA", "5-MA", "7-MA"),col=c("blue", "red", "green"),bty="n",cex=1.0,horiz=F,lwd=1)


library(fpp)
library(fpp2)
library(urca)
DATA1 = c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28,1.78, 2.87, 3.88, 1.4, 2.37,  3.42, 4.7, 1.86, 2.66, 3.83, 5.14, 2.46,
          3.94, 4.82, 6.87, 2.19, 3.38, 5.81, 5.27, 4.68, 4.98, 6.62)
DATA3 = c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28,1.78, 2.87, 3.88, 1.4, 2.37,  3.42, 4.7, 1.86, 2.66, 3.83, 5.14, 2.46,
          3.94, 4.82, 6.87, 2.19, 3.38, 5.81, 5.27, 4.68, 4.98, 6.62,8.26, 5.45, 6.07, 8.84, 9.6, 2.7, 6.77)
summary(DATA3) #provides summary statistics of overall data
subscribers = ts(DATA1,start=2011,frequency=4) #training data converted to time series
subscribers 
DATA2 = c(8.26, 5.45, 6.07, 8.84, 9.6, 2.7, 6.77) #testing data
subscribers2 = ts(DATA2,start=2018,frequency=4) #testin data converted to time series
summary(DATA1) #summary stats on training
summary(DATA2) #summary stats on testing
plot(subscribers) 

#Detect seasonality/trend using Decomposition
fit = stl(subscribers,s.window=5)
fit
plot(subscribers, main = "Additional Paid Netflix Subscribers (2011-2017)", ylab = "Number of Subscribers", xlab = "Year", col = "grey")
plot(fit,col = "blue", main = "Decomposition of Netflix Additional Paid Subscribers")
lines(fit$time.series[,2],col="purple",ylab="Trend") #shows trend component
lines(fit$time.series[,1],col="red",ylab="Seasonality") #shows seasonality
lines(fit$time.series[,3],col="blue",ylab="Error") #shows error
legend("topleft",legend=c("Trend","Seasonality","Error"),col=c("purple","red","blue"),bty="n",cex=1.0,horiz=F,lwd=1)
lines(seasadj(fit),col="blue") #shows without seasonality (trend + error)
legend("topleft",legend=c("Seasonally Adjusted Data"),col=c("blue"),bty="n",cex=1.0,horiz=F,lwd=1)
#Looks like there is a long term increase, and seasonality (Q1 and Q4)? 


#Holt-Winters Seasonal Method
#additive model - seasonal variations are roughly constant through the series (likely)
#multiplicative model - seasonal variations are changing proportional to the level of the series
#####ADDITIVE MODEL#########
fit1 = hw(subscribers,seasonal="additive")
fit1
fit1$model #shows AIC/BIC
plot(subscribers,main="Additional Paid Netflix Subscribers",ylab="Number of Subscribers (in millions)",xlab="Year")
fitted(fit1) #find fitted values
lines(fitted(fit1),col="red",type="o")
legend("topleft",legend=c("Fitted Values"),col=c("red"),bty="n",cex=1.0,horiz=F,lwd=1)

#####MULTIPLICATIVE MODEL###### - not used in presentation b/c it cannot be compiled due to negative numbers.
fit2 = hw(subscribers,seasonal="multiplicative")
fit2
plot(subscribers,main="Number of Netflix Subscribers",ylab="Number of Subscribers (in millions)",xlab="Year")
fitted(fit2) #find fitted values
lines(fitted(fit2),col="purple",type="o")
########Holt Winters Plot##############
autoplot(subscribers)+autolayer(fit1,series="Holt Winters Additive Forecasts",PI=FALSE)+xlab("Year")+ylab("New Subscribers (in millions)")+
  ggtitle("Netflix's Additional Paid Subscriptions")+guides(colour=guide_legend(title="Forecast"))



#########Unit Root Test#############
#determine if differencing is required 
test = ur.kpss(subscribers)
summary(test)
#test-stat:0.8824 > 5% level of significance: 0.463 - differencing required
ndiffs(subscribers) #2 levels of differencing
nsdiffs(subscribers) #no seasonal differences if Fs < 0.64, 0<0.64
#first diff
first_diff = diff(subscribers)
first_diff
test2 = ur.kpss(first_diff)
summary(test2)
#t-stat: 1.0295 > 0.463
#second diff
second_diff = diff(first_diff)
second_diff
test3 = ur.kpss(second_diff)
summary(test3)
#test-stat: 0.1109 < 0.463 (5% level of sig)
#verify's values obtained by ndiffs 


######ARIMA MODEL#######################
#need to determine optimal values for p,d,q
fit4 = auto.arima(subscribers,seasonal=FALSE)
fit4
#Arima model 3,1,0 is optimal 
fit5 = Arima(subscribers, order=c(3,1,0))
fit5
fitted(fit5)
plot(subscribers, main = "Netflix Subscribers (2011-2018)", ylab = "Number of Subscribers", xlab = "Year", col = "grey")
lines(fitted(fit5),col="green")
fc2 = forecast(fit5, h = 7)
fc2
lines(fc2$mean,col="blue") #no blue showing up?


#Holts-Winter on Testing Data
accuracy(fit1,DATA2)
accuracy(fit1)
average(accuracy(fit1,DATA2))




library(fpp2)
library(fpp)
library(urca)

#training data: 2011Q1 to 2017Q4 (28 data points)
subscribers<-c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28, 1.78, 2.87, 3.88, 1.4, 2.37, 3.42, 4.7, 1.86, 2.66,
               3.83, 5.14, 2.46, 3.94, 4.82, 6.87, 2.19, 3.38, 5.81, 5.27, 4.68, 4.98, 6.62)

#testing data: 2018Q1 to 2019Q3 (7 data points)
subscribersTest<-c(8.26, 5.45, 6.07, 8.84, 9.6, 2.7, 6.77) 

#convert data into time series data
subscribers<-ts(subscribers, start = 2011, frequency = 4 )
subscribersTest<-ts(subscribersTest, start =2018, frequency = 4)

summary(subscribersTest)

plot(subscribers)

#Simple Forecasting Methods (see Adam's code for plots of these forecasting methods)
subFitMean <- meanf(subscribersTrain,h=7)
subFitMean
subFitNaive <- rwf(subscribersTrain,h=7)
subFitNaive
subFitSNaive <- snaive(subscribersTrain,h=7)
subFitSNaive
subFitDrift <- rwf(subscribersTrain,h=7, drift=TRUE)
subFitDrift

accuracy(subFitMean, subscribersTest)
accuracy(subFitNaive, subscribersTest)
accuracy(subFitSNaive, subscribersTest)
accuracy(subFitDrift, subscribersTest)

plot(subFitMean, main = "Worldwide Change in Subscriber Forecast: Average Method", ylab = "Number of Subscribers (Millions)", xlab = "Year")


################# ARIMA #################################
subscribers<-ts(subscribers, start = 2011, frequency = 4 )
subscribersTest<-ts(subscribersTest, start =2018, frequency = 4)
subscribers
plot(subscribers, ylab="Subscribers Added (Millions)", main="Arima Method")
fitSubscribers<-auto.arima(subscribers,seasonal = TRUE)
fitSubscribers
#(3, 1, 0) is best model (meaning not useful to use previous errors at all?)
ndiffs(subscribers) 

futureVals<-forecast(fitSubscribers, h=7)
futureVals

accuracy(futureVals, subscribersTest)

fit = Arima(subscribers, order = c(3,1,0))
fit 

fitted(fitSubscribers) #find the fitted values 
lines(fitted(fit), col='red') 
fc=forecast(fit, h=70)
fc
accuracy(fc)
#need Arima model 
plot(subscribers)
lines(fitted(fit), col='red') 
lines(fc$mean, col='blue')

accuracy(fit)
accuracy(fc, subscribersTest)

fit2 = Arima(subscribersTest, order = c(3,1,0))
fit2
accuracy(fit2)

############# KPSS TESTING ###################
Test=ur.kpss(subscribers)
summary(Test)


diffOne<-diff(subscribers)
Test2=ur.kpss(diffOne)
summary(Test2)

#We also performed the KPSS test where our t-stat was 0.9022, which is greater than 0.463 
#at the 5% significance level → this meant that we had to perform the first differencing, 
#and after this we received a t-stat of 0.072, which was less than the 5% significance 
#value of 0.563 (we also used the ndiffs function to verify that the first differencing 
#was enough) 




library(fpp2)
library(fpp)
library(urca)

#total data set 
totalSubscribers<-c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28, 1.78, 2.87, 3.88, 1.4, 2.37, 3.42, 4.7, 1.86, 2.66,
                    3.83, 5.14, 2.46, 3.94, 4.82, 6.87, 2.19, 3.38, 5.81, 5.27, 4.68, 4.98, 6.62,8.26, 5.45, 6.07, 8.84, 9.6, 2.7, 6.77)
totalSubscribers<-ts(totalSubscribers, start = 2011, frequency = 4)
totalSubscribers

#training data: 2011Q1 to 2017Q4 (28 data points)
subscribersTrain<-c(1.39, 1.9, -1.8, 0.1, 2.83, 1.28, 1.78, 2.87, 3.88, 1.4, 2.37, 3.42, 4.7, 1.86, 2.66,
                    3.83, 5.14, 2.46, 3.94, 4.82, 6.87, 2.19, 3.38, 5.81, 5.27, 4.68, 4.98, 6.62)
subscribersTrain<-ts(subscribersTrain, start=2011, frequency = 4)
subscribersTrain

#testing data: 2018Q1 to 2019Q3 (7 data points)
subscribersTest<-c(8.26, 5.45, 6.07, 8.84, 9.6, 2.7, 6.77) 
subscribersTest<-ts(subscribersTest, start = 2018, frequency = 4)
subscribersTest

#plot of data
autoplot(totalSubscribers) +
  ggtitle("Additions in Netflix Paid Subscriptions (Millions)") +
  ylab("Paid Subscriptions Added (Millions)") +
  xlab("Year")

#seasonal plot 
ggseasonplot(totalSubscribers, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Subscriptions Added (Millions)") +
  ggtitle("Seasonal Plot: Additions in Netflix Paid Subscriptions (Millions) ")

#seasonal subseries plot 
ggsubseriesplot(totalSubscribers) +
  ylab("Subscribers Added (Millions)") +
  ggtitle("Seasonal Subseries Plot: Additions in Netflix Paid Subscriptions (Millions)")

#Summary plot of the different forecasting methods
autoplot(subscribersTrain) +
  autolayer(meanf(subscribersTrain, h=7),
            series="Mean", PI=FALSE) +
  autolayer(naive(subscribersTrain, h=7),
            series="Naive", PI=FALSE) +
  autolayer(snaive(subscribersTrain, h=7),
            series="Seasonal naive", PI=FALSE) +
  autolayer(rwf(subscribersTrain, h=7, drift = TRUE),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for Additions in Netflix Paid Subscriptions") +
  xlab("Year") + ylab("Additions in Subscriptions (Millions)") +
  guides(colour=guide_legend(title="Forecast"))


plot(totalSubscribers,col="gray", main = "Subscribers Added", ylab = "Additions in Subs", xlab = "Time")
fit <- stl(totalSubscribers, s.window=5)

#s.window is a paramter
#FIX DATA
fit
lines(fit$time.series[,2], col = "red", ylab = "Trend")
lines(fit$time.series[,1], col = "blue", ylab = "Seasonal")
lines(fit$time.series[,3], col = "green", ylab = "Error")
plot(fit)