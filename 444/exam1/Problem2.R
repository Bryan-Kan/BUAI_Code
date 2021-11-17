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

hotelOccupancyforecasts<-ets(hotelOccupancySeries,model="AAA", damped=FALSE)
hotelOccupancyforecasts
hotelOccupancyforecasts$fitted
plot(hotelOccupancyforecasts)
accuracy(hotelOccupancyforecasts)
residuals(hotelOccupancyforecasts)
summary(hotelOccupancyforecasts)
coef(hotelOccupancyforecasts)
forecast(hotelOccupancyforecasts, level=0.95, h=24)


hotelOccupancyforecasts<-ets(hotelOccupancySeries, model="MAM", damped = F)
hotelOccupancyforecasts
hotelOccupancyforecasts$fitted
plot(hotelOccupancyforecasts)
accuracy(hotelOccupancyforecasts)
residuals(hotelOccupancyforecasts)
summary(hotelOccupancyforecasts)
coef(hotelOccupancyforecasts)
forecast(hotelOccupancyforecasts, level=0.95, h=24)

fit <- auto.arima(hotelOccupancySeries,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')
summary(fit)
accuracy(fit)
coef(fit)
forecast(fit,h=24)
plot(forecast(fit,h=24), include=1,24)
str(fit)


