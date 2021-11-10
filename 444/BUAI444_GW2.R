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

hw2forecasts<-ets(hw2series,model="AAA", lower = c(0.1,0.1,0.1, 0.8), upper = c(0.5,0.5,0.5, 0.9), damped=FALSE)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)
#RMSE 14.23307

hw2forecasts<-ets(hw2series, model="MAM",lower = c(0.1,0.1,0.1, 0.8), upper = c(0.5,0.5,0.5, 0.9), damped = F)
hw2forecasts
hw2forecasts$fitted
plot(hw2forecasts)
accuracy(hw2forecasts)
residuals(hw2forecasts)
summary(hw2forecasts)
coef(hw2forecasts)
forecast(hw2forecasts, level=0.95, h=12)
predict(hw2forecasts,level=0.95, h=12)

