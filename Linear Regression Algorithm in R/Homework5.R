# Start Fresh 
rm(list=ls())
#Load the GGally library for ggpairs()
library(GGally)
#Read the data
uscrime <- read.table("uscrime.txt",stringsAsFactors = FALSE,header = TRUE)
summary(uscrime)
hist(uscrime$Crime)

#seams like logarithmic transformation.
#If  the two highest are outliers we can remove them
# before running the outliers , check for normality

#fit a linear regression model
linearmodel_uscrime <- lm(Crime~.,data = uscrime)
linearmodel_uscrime
vif(linearmodel_uscrime)
summary(linearmodel_uscrime)
hist(linearmodel_uscrime$residuals)
#we need to determine if this is normally distributed or not.
# The residuals are normally distributed with the mean of zero.
# that is why plotted to see if its true.
# but shows not so true

testpt <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,
                     Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150,
                     NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,
                     Ineq = 20.1,Prob = 0.04,Time = 39.0)

#Predict the crime rate for the data point
predict_model <-predict(linearmodel_uscrime,testpt)
predict_model
#Is this a good prediction?
qqnorm(uscrime$Crime)
range(uscrime$Crime)

#The prediction value is out of range i.e less than the minimum value of 342 in the crime Range. this indicates
#that may be we are overfitting the data. So lets check!

# Assumption is errors are normally distributed.
#relationship between the variables
pairs(uscrime[,12:16])
cor(uscrime[,12:16])

ggpairs(uscrime[,12:16])
#TO avoid colinearity which of these will you take out 
# we can take out wealth or ineq 
#we want predictors not to be correlated as the algothim is unstable
#and it assigns different magnitudes of the coefficents as 
#they are way similar.

pairs(uscrime[,5:10])
cor(uscrime[,5:10])

ggpairs(uscrime[,5:10])


#minimum is not even close to 155. Something is wrong with the prediction
# Looks like it is overfit.
# Let is try to remove some variables and try again to provide 
# a qualitier fit

library(DAAG) 

#This library has a cv.lm() to get more accurate measure of the qualitier fit


#perform 4-fold CV with the linear model that was created earlier

set.seed(42)
linearmodel_uscrime_cv<-cv.lm(uscrime,linearmodel_uscrime,m=4)

# Let us calculate the Rsquared error 
sse<-94720*nrow(uscrime)
sst<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq<-1-sse/sst
rsq

#How many variabes - 10 datapts for 1 as we have 48 5 will be good
summary(linearmodel_uscrime)
# I am planning to remove the insignificant variables which are above
# the threshold = 0.1
# Removing So,Po2,LF,M.F,Pop,NW,U1,Wealth,Time

BetterModel<-lm(Crime~M+Ed+Po1+U2+Ineq+Prob,uscrime)
summary(BetterModel)
hist(BetterModel$residuals)
set.seed(42)
BetterModel_cv<-cv.lm(uscrime,BetterModel,m=4)

vif(BetterModel)

sse<-48203*nrow(uscrime)
sst<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq<-1-sse/sst
rsq
plot(linearmodel_uscrime)

