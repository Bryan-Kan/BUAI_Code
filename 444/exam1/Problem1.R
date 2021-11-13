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


#Testing Homoscedasticity
plot(causalreg$fitted.values, causalreg$residuals)

#Testing Linearity
zres<-rstandard(causalreg)
plot(causaldata$INT, zres)


#Test for Normality
hist(causalreg$residuals)
qqnorm(causalreg$residuals)

shapiro.test(causalreg$residuals)

#Test of Independence
data<-data.frame(QTR=c(1:40))
newcausaldata<-cbind(causaldata,data)
#newdata
plot(newcausaldata$QTR, causalreg$residuals, type="b")

install.packages("lmtest")
library("lmtest")
dwtest(causalreg, alternative = "two.sided")
