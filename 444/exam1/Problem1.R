#install.packages("readxl")
library("readxl")
beltdata <- read_excel("/Users/bryankan/Downloads/belt.xlsx")
beltdata
nrow(beltdata)
plot(beltdata)

#Stepwise Regression
null<-lm(TIME ~ 1,data=beltdata)
full<-lm(TIME ~ LENGTH+WIDTH+SPLICE,data=beltdata)

stepreg<-step(null,scope=list(lower=null,upper=full),direction="both")
summary(stepreg)

#Detecting Ouliers
rstandard(stepreg)
stepreg$residuals
rstandard(stepreg)
cook<-cooks.distance(stepreg)
cook
plot(cook,ylab="Cooks Distance")
points(17,cook[17],col='red')



#Testing Homoscedasticity
par(mfrow=c(1,2))
plot(stepreg$fitted.values, stepreg$residuals)
zres<-rstandard(stepreg)
plot(stepreg$fitted.values, zres)


#Testing Linearity
par(mfrow=c(1,3))
plot(beltdata$TIME,zres)
plot(beltdata$LENGTH, zres)
plot(beltdata$WIDTH, zres)


#Test for Normality
par(mfrow = c(1,2))
hist(stepreg$residuals)
qqnorm(stepreg$residuals)
shapiro.test(stepreg$residuals)


#Test of Independence
data<-data.frame(QTR=c(1:21))
newbeltdata<-cbind(beltdata,data)
#newdata
plot(newbeltdata$QTR, stepreg$residuals, type="b")

library("lmtest")
dwtest(stepreg, alternative = "two.sided")


data<-data.frame(LENGTH=c(250), WIDTH=c(15),SPLICE=c("Y"))
pred.int<-predict(stepreg,data,level=0.95, interval="prediction")
newbeltdata<-cbind(data,pred.int)
newbeltdata



