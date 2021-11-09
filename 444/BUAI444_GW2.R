# Read Data
hw2.csv = read.csv("HW2.csv", header=T)
hw2.csv
hw2.csv$Xt
hw2.Xt <- hw2.csv$Xt
hw2.Xt

#install package (Forecast)

install.packages("forecast")
library("forecast")

#Define radioseries as a timeseries
hw2series=ts(hw2.Xt, frequency=12, start = c(2012,1))
hw2series
plot.ts(hw2series)

#Additive Holt-Winters Model
hw2forecasts<-ets(hw2series,model="AAA", damped=FALSE)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)

#Multiplicative Holt-Winters
hw2forecasts<-ets(hw2series, model="MAM", damped = F)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)


#Multiplicative Holt-Winters with particular values for alpha, beta, and gamma
hw2forecasts<-ets(hw2series, model="MAM", alpha = 0.2, beta = 0.05, gamma = 0.2, damped = F)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)


#Multiplicative Holt-Winters with range for alpha, beta, and gamma
hw2forecasts<-ets(hw2series, model="MAM", lower = c(0.1,0.1,0.1, 0.8), upper = c(0.5,0.5,0.5, 0.9), damped = F)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)



#Using Training Set
hw2.training <- window(hw2series,start=c(2012,1), end=c(2019,12))
hw2.test<-window(hw2series, start=c(2012,1))
hw2.training
hw2.test





#Additive Holt-Winters Model
hw2forecasts<-ets(hw2.training,model="AAA", damped=FALSE)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
fcast<-forecast(hw2forecasts, level=0.95, h=24)
predict(hw2forecasts,level=0.95, h=24)

accuracy(fcast, hw2.test)



