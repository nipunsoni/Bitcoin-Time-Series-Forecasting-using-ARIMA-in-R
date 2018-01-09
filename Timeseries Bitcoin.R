library(TSA)
library(tseries)
library(forecast)
library(lubridate)
library(zoo)

getwd()

#load data
btc.data = read.csv(file.choose(), stringsAsFactors = FALSE)

btc.data.ts = ts(data=btc.data$Adj.Close, frequency = 12,
             start=c(2012,01), end=c(2017,12))

class(btc.data.ts)


#start of ts
start(btc.data.ts)

#end of ts
end(btc.data.ts)

#check frequency of ts
frequency(btc.data.ts)

#check summary
summary(btc.data.ts)

#plot data to see how it looks
plot(btc.data.ts)

#print cycles of data across years
cycle(btc.data.ts)

#see trends in data
plot(decompose(btc.data.ts))

#draw a regression line
abline(reg = lm(btc.data.ts ~ time(btc.data.ts)))

#display YOY trend of mean
plot(aggregate(btc.data.ts,FUN = mean))

#see box plot. Helps to understand seasonality in a way
boxplot(btc.data.ts ~ cycle(btc.data.ts))

#Make data stationary by making mean and variance constant
#Taking log makes variance constant and differentiating (using diff function) data addresses mean 
#and seasonal trend in data

#dealing with variance
plot(log(btc.data.ts))

#dealing with mean
plot(diff(log(btc.data.ts)))

#Stationary check (Dicky-Fuller test)
adf.test(btc.data.ts, alternative = "stationary")

################## ARIMA Model #####################

# AR I MA
# p  d q

#auto-correlation function and partial auto-correlation function graph with raw data
acf(btc.data.ts)
pacf(btc.data.ts)

#Using auto.arima
auto.arima(btc.data.ts)

#Applying ARIMA model
#c(p, d, q)

fit <- arima(log(btc.data.ts), c(1,2,0), seasonal = list(order = c(1,2,0), period = 12))
fit

#predict for next 10 years
predict <- predict(fit, n.ahead = 2 * 12)
predict

#take exp since log was taken initially
predict <- exp(predict$pred)
predict

#plot results
ts.plot(btc.data.ts, predict, log = "y", lty = c(1,3))