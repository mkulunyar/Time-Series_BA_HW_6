getwd()
setwd('/Users/mkulunyar/Dropbox/NYU/Fall16/BusinessAnalytics/HW_6')

#Data Export
#investing.com-source
oil<-read.csv('new_oil_daily.csv', header=TRUE, stringsAsFactors=FALSE)
gold<-read.csv('new_gold_monthly.csv', header=TRUE, stringsAsFactors=FALSE)
sp500<-read.csv('new_sp500_daily.csv', header=TRUE, stringsAsFactors=FALSE)

#DATE
#Date Modification for Oil
head(oil)
oil$Date <- as.Date(oil$Date, "%d-%B-%y")
oil_sorted<-oil[order(oil$Date, decreasing = FALSE),]
head(oil_sorted)
#Date Modification for sp500
head(sp500)
sp500$Date <- as.Date(sp500$Date, "%d-%B-%y")
sp500_sorted<-sp500[order(sp500$Date, decreasing = FALSE),]
head(sp500_sorted)
#Date Modification for gold
head(gold)
gold$Date <- as.Date(gold$Date, "%y-%B-%d")
gold_sorted<-gold[order(gold$Date, decreasing = FALSE),]
head(gold_sorted)

#Time Series
#Oil
#Daily
summary(oil_sorted$Price)
Crude_Oil.ts <- ts(oil_sorted$Price, frequency = 365, start=c(2011))
plot(Crude_Oil.ts)
#Monthly
head(oil_sorted)
oil_sorted$MonthYear<-format(oil_sorted$Date, format="%Y-%m")
oil_monthly <- aggregate(oil_sorted$Price ~ MonthYear, oil_sorted, mean, na.rm=TRUE)
oil_monthly
oil_monthly.ts<-ts(oil_monthly$`oil_sorted$Price`, frequency = 12, start = c(2011))
plot(oil_monthly.ts)
#Gold
summary(gold_sorted$Price)
Gold.ts <- ts(gold_sorted$Price, frequency = 12, start=c(2011))
plot(Gold.ts)

#sp500
#Daily
summary(sp500_sorted$Price)
sp500.ts <- ts(sp500_sorted$Price, frequency = 365, start=c(2011))
plot(sp500.ts)
#Monthly
head(sp500_sorted)
sp500_sorted$MonthYear<-format(sp500_sorted$Date, format="%Y-%m")
sp500_monthly <- aggregate(sp500_sorted$Price ~ MonthYear, sp500_sorted, mean, na.rm=TRUE)
sp500_monthly
sp500_monthly.ts<-ts(sp500_monthly$`sp500_sorted$Price`, frequency = 12, start = c(2011))
plot(sp500_monthly.ts)

#Decomposition
#Gold
Gold.ts.d<-decompose(Gold.ts)
plot(Gold.ts.d)
#Oil
Crude_Oil.ts.d<-decompose(oil_monthly.ts)
#sp500
sp500.ts.d<-decompose(sp500_monthly.ts)
plot(sp500.ts.d)

#Combining Oil, Gold and sp500
library(xts)
#Oil-Gold
oil_gold <- cbind(oil_monthly.ts, Gold.ts)
plot(oil_gold)


cor(Gold.ts, oil_monthly.ts)
require(graphics)
#Cross Correlation Plots, Lets see what effects oil prices
ccf(Gold.ts, oil_monthly.ts, ylab = "cross-correlation")
ccf(sp500_monthly.ts, oil_monthly.ts, ylab = "cross-correlation")
ccf(sp500_monthly.ts, Gold.ts, ylab = "cross-correlation")

#gold-sp500
gold_sp500 <- cbind(Gold.ts, sp500_monthly.ts)
plot(gold_sp500)
#oil-sp500
oil_sp500 <- cbind(oil_monthly.ts, sp500_monthly.ts)
plot(oil_sp500)
#Some Visuals
ts.plot(oil_monthly.ts, Gold.ts,gpars= list(col=rainbow(10)))
ts.plot(oil_monthly.ts, sp500_monthly.ts,gpars= list(col=rainbow(10)))
ts.plot(Gold.ts, sp500_monthly.ts,gpars= list(col=rainbow(10)))


#FORECASTING
#oil
#smoothing
oil_monthly.holt.T <- HoltWinters(oil_monthly.ts, gamma=TRUE)
oil_monthly.holt.T
plot(oil_monthly.holt.T)
oil_monthly.holt.F <- HoltWinters(oil_monthly.ts, gamma=FALSE)
oil_monthly.holt.F
plot(oil_monthly.holt.F)
#forecasting, holt, horizon is 12 months
library(forecast)
oil_monthly.forecasts <- forecast.HoltWinters(oil_monthly.holt.T, h=12)  
plot.forecast(oil_monthly.forecasts)
oil_monthly.forecasts <- forecast.HoltWinters(oil_monthly.holt.F, h=12)  
plot.forecast(oil_monthly.forecasts)
#ARIMA
oil_monthly.arima<-arima(oil_monthly.ts, c(0,0,0))  
oil_monthly.arima.forecasts <- forecast.Arima(oil_monthly.arima, h=12)
oil_monthly.arima.forecasts
plot(oil_monthly.arima.forecasts)

oil_monthly.arima<-arima(oil_monthly.ts, c(0,1,0))  
oil_monthly.arima.forecasts <- forecast.Arima(oil_monthly.arima, h=12)
oil_monthly.arima.forecasts
plot(oil_monthly.arima.forecasts)

oil_monthly.arima<-arima(oil_monthly.ts, c(1,1,0))  
oil_monthly.arima.forecasts <- forecast.Arima(oil_monthly.arima, h=12)
oil_monthly.arima.forecasts
plot(oil_monthly.arima.forecasts)

#gold
#smoothing
Gold.holt.T <- HoltWinters(Gold.ts, gamma=TRUE)
Gold.holt.T
plot(Gold.holt.T)
Gold.holt.F <- HoltWinters(Gold.ts, gamma=FALSE)
Gold.holt.F
plot(Gold.holt.F)
#forecasting, holt, horizon is 12 months
library(forecast)
Gold.forecasts <- forecast.HoltWinters(Gold.holt.T, h=12)  
plot.forecast(Gold.forecasts)
Gold.forecasts <- forecast.HoltWinters(Gold.holt.F, h=12)  
plot.forecast(Gold.forecasts)
#ARIMA
Gold.arima<-arima(Gold.ts, c(0,0,0))  
Gold.arima.forecasts <- forecast.Arima(Gold.arima, h=12)
Gold.arima.forecasts
plot(Gold.arima.forecasts)

Gold.arima<-arima(Gold.ts, c(0,1,0))  
Gold.arima.forecasts <- forecast.Arima(Gold.arima, h=12)
Gold.arima.forecasts
plot(Gold.arima.forecasts)

Gold.arima<-arima(Gold.ts, c(1,1,0))  
Gold.arima.forecasts <- forecast.Arima(Gold.arima, h=12)
Gold.arima.forecasts
plot(Gold.arima.forecasts)


#sp500
#smoothing
sp500_monthly.holt.T <- HoltWinters(sp500_monthly.ts, gamma=TRUE)
sp500_monthly.holt.T
plot(sp500_monthly.holt.T)
sp500_monthly.holt.F <- HoltWinters(sp500_monthly.ts, gamma=FALSE)
sp500_monthly.holt.F
plot(sp500_monthly.holt.F)
#forecasting, holt, horizon is 12 months
library(forecast)
sp500_monthly.forecasts <- forecast.HoltWinters(sp500_monthly.holt.T, h=12)  
plot.forecast(sp500_monthly.forecasts)
sp500_monthly.forecasts <- forecast.HoltWinters(sp500_monthly.holt.F, h=12)  
plot.forecast(sp500_monthly.forecasts)
#ARIMA
sp500_monthly.arima<-arima(sp500_monthly.ts, c(0,0,0))  
sp500_monthly.arima.forecasts <- forecast.Arima(sp500_monthly.arima, h=12)
sp500_monthly.arima.forecasts
plot(sp500_monthly.arima.forecasts)

sp500_monthly.arima<-arima(sp500_monthly.ts, c(0,1,0))  
sp500_monthly.arima.forecasts <- forecast.Arima(sp500_monthly.arima, h=12)
sp500_monthly.arima.forecasts
plot(sp500_monthly.arima.forecasts)

sp500_monthly.arima<-arima(sp500_monthly.ts, c(1,1,0))  
sp500_monthly.arima.forecasts <- forecast.Arima(sp500_monthly.arima, h=12)
sp500_monthly.arima.forecasts
plot(sp500_monthly.arima.forecasts)