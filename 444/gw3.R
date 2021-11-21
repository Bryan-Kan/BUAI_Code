# load package 
#install.packages("readxl")
library("readxl")
library(tidyverse)
library(ggplot2)
library(ggfortify)
install.packages("psych")
library(psych)
install.packages("graphics")
library(graphics)

# import data
universitiesData <- read_excel("/Users/bryankan/Downloads/Universities-2.xlsx")

head(universitiesData)

universitiesData$`in-state tuition`[is.na(universitiesData$`in-state tuition`)]<-median(universitiesData$`in-state tuition`,na.rm=TRUE)
universitiesData$`out-of-state tuition`[is.na(universitiesData$`out-of-state tuition`)]<-median(universitiesData$`out-of-state tuition`,na.rm=TRUE)
universitiesData$room[is.na(universitiesData$room)]<-median(universitiesData$room,na.rm=TRUE)
universitiesData$board[is.na(universitiesData$board)]<-median(universitiesData$board,na.rm=TRUE)
universitiesData$`add. fees`[is.na(universitiesData$`add. fees`)]<-median(universitiesData$`add. fees`,na.rm=TRUE)
universitiesData$`estim. book costs`[is.na(universitiesData$`estim. book costs`)]<-median(universitiesData$`estim. book costs`,na.rm=TRUE)
universitiesData$`estim. personal $`[is.na(universitiesData$`estim. personal $`)]<-median(universitiesData$`estim. personal $`,na.rm=TRUE)
universitiesData$`stud./fac. ratio`[is.na(universitiesData$`stud./fac. ratio`)]<-median(universitiesData$`stud./fac. ratio`,na.rm=TRUE)
universitiesData$`Graduation rate`[is.na(universitiesData$`Graduation rate`)]<-median(universitiesData$`Graduation rate`,na.rm=TRUE)
universitiesData$`# appli. rec'd`[is.na(universitiesData$`# appli. rec'd`)]<-median(universitiesData$`# appli. rec'd`,na.rm=TRUE)
universitiesData$`# appl. accepted`[is.na(universitiesData$`# appl. accepted`)]<-median(universitiesData$`# appl. accepted`,na.rm=TRUE)
universitiesData$`# new stud. enrolled`[is.na(universitiesData$`# new stud. enrolled`)]<-median(universitiesData$`# new stud. enrolled`,na.rm=TRUE)
universitiesData$`% new stud. from top 10%`[is.na(universitiesData$`% new stud. from top 10%`)]<-median(universitiesData$`% new stud. from top 10%`,na.rm=TRUE)
universitiesData$`% new stud. from top 25%`[is.na(universitiesData$`% new stud. from top 25%`)]<-median(universitiesData$`% new stud. from top 25%`,na.rm=TRUE)
universitiesData$`# FT undergrad`[is.na(universitiesData$`# FT undergrad`)]<-median(universitiesData$`# FT undergrad`,na.rm=TRUE)
universitiesData$`# PT undergrad`[is.na(universitiesData$`# PT undergrad`)]<-median(universitiesData$`# PT undergrad`,na.rm=TRUE)
universitiesData$`% fac. w/PHD`[is.na(universitiesData$`% fac. w/PHD`)]<-median(universitiesData$`% fac. w/PHD`,na.rm=TRUE)

universitiesData$`% new stud. from top 10%` <- universitiesData$`% new stud. from top 10%` / 100
universitiesData$`% new stud. from top 25%` <- universitiesData$`% new stud. from top 25%` / 100
universitiesData$`% fac. w/PHD` <- universitiesData$`% fac. w/PHD` / 100


length(universitiesData)

universitiesData

# Before computing pcs, I can generate correlation matrix 
# to show it to the student.  And the covariance matrix. 
# Just to teach them how to evaluate the data even before the run PCA
cor(universitiesData[,-c(1:2)])
cov(universitiesData[,-c(1:2)])


# Compute PCs on two dimensions
pcs <- prcomp(data.frame(universitiesData$, cereals$rating))
summary(pcs)




# The function rot gives us the weights-the original variables contribution
# to the principal components
pcs$rot



# Here we get the scores for the newly create principal componants
head(scores, 5)

# Scatter plot for the first vs. second principal scores
# The scale=0 argument to biplot() ensures that the arrows are scaled
# to biplot() represent the loadings; other values for scale give
# slightly different biplots with different interpretations.
biplot(pcs, scale=0)


# To get PVE (Proportional variance explained)

# Find the standard deviation of each principal component
pcs$sdev

# Find The variance explained by each principal component
pcs.var=pcs$sdev^2
pcs.var

# Compute the proportion of variance explained by each principal component
pve=pcs.var/sum(pcs.var)
pve

options(DECIMAL=4)
options(scipen = 999)

# We see that the first principal component explains 54.0% of the variance
# in the data, the next principal component explains 38.7% of the variance,
# and so forth.
plot(pve, 
     xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')

plot(cumsum(pve),
     xlab="Principal Component",
     ylab=" Cumulative Proportion of Variance Explained ",
     ylim=c(0,1) ,type='b')


write.csv(scores, file = "scores.csv")





# By using na.omit(), we remove observations that contain missing values

# Compute PCs on all 13 variables
pcs13 <- prcomp(na.omit(cereals[,-c(1:3)]))
summary(pcs13)
# The rotation matrix provides the principal component loadings
# Each column contains the corresponding principal component loading vector.
pcs13$rot
pcs13$rot [,1:5]




biplot(pcs13, scale=0) 



#  loadings = TRUE draws eigenvectors.
autoplot(pcs13, loadings = TRUE, )
autoplot(pcs13, loadings = TRUE, loadings.label = TRUE, )



screeplot(pcs13, type = "l" )
screeplot(pcs13, type = "l", main = "Screeplot")

# type=“b” for both points are connected by a line. 




# Compute PCs on all 13 variables after standardization
# By using the option scale=TRUE, we scale the variables to have standard deviation one

pcs.sdr <- prcomp(na.omit(cereals[,-c(1:3)]), scale. = T)
summary(pcs.sdr)
pcs.sdr$rot
pcs.sdr$rot [,1:5]
scores <- pcs.sdr$x

dim(pcs.sdr$x)




# Find the standard deviation of each principal component
pcs.sdr$sdev

# Find The variance explained by each principal component
pcs13.var=pcs.sdr$sdev^2
pcs13.var

# Compute the proportion of variance explained by each principal component
pve=pcs13.var/sum(pcs13.var)
pve

options(DECIMAL=4)
options(scipen = 999)


plot(pve, 
     xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')

plot(cumsum(pve),
     xlab="Principal Component",
     ylab=" Cumulative Proportion of Variance Explained ",
     ylim=c(0,1) ,type='b')



# Save the new dataset
write.csv(scores, file = "scores.csv")


