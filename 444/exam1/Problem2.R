# Read Data
hotel_Occupancy.csv = read.csv("Hotel_occupancy.csv", header=T)
hotel_Occupancy.csv 
hotel_Occupancy.csv$Rooms
hotel_Occupancy.Rooms <- hotel_Occupancy.csv$Rooms
hotel_Occupancy.Rooms

#install package (Forecast)

install.packages("forecast")
library("forecast")

#Define radioseries as a timeseries
hotelOccupancySeries=ts(hotel_Occupancy.Rooms, frequency=12, start = c(2007,1))
hotelOccupancySeries
plot.ts(hotelOccupancySeries)

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

