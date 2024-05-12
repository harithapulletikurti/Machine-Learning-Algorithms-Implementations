# Start Fresh 
rm(list=ls())
#Load the GGally library for ggpairs()
library(GGally)
library(car)
library(DAAG)

#Read the data
uscrime <- read.table("uscrime.txt",stringsAsFactors = FALSE,header = TRUE)
summary(uscrime)
hist(uscrime$Crime)

#For Multiple linear regression model  using least squares we can use the lm() function.
#The crime data set has 15 variables and the 16th variable Crime is used as response.

lm_crime <- lm(Crime~.,data = uscrime)
summary(lm_crime)
RSquared = summary(lm_crime)$r.sq
RSE = summary(lm_crime)$sigma

hist(lm_crime$residuals)

#we need to determine if this is normally distributed or not.
# The residuals are normally distributed with the mean of zero.
# that is why plotted to see if its true.
# but shows not so true

testpt <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,
                     Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150,
                     NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,
                     Ineq = 20.1,Prob = 0.04,Time = 39.0)

#Predict the crime rate for the data point
predict_model <-predict(lm_crime,testpt)
predict_model


#Is this a good prediction?
qqnorm(uscrime$Crime)
range(uscrime$Crime)

#the predict value is not in the range of uscrime. 
#it is far below the lower bound. So this shows that our regression model is 
# not the best and may be over fitting rhe data 

#compute variance inflation factors using vif
vif(lm_crime)

#Based on this result, the p-value  is very high for the following factors
#Po2,Po1,Wealth,Ineq,So,Ed,M.f,L.f
# let us remove one by one and check the regression model.

lm_model_1 =lm(Crime~.-Po2,data = uscrime)
summary(lm_model_1)$r.sq
summary(lm_model_1)$sigma

#Predict the crime rate for the data point
predict_model_1 <-predict(lm_model_1,testpt)
predict_model_1
range(uscrime$Crime)

# The predicted value is within the range
plot(lm_model_1)
set.seed(42)
options(warn=-1)
lm_model_1_cv<-cv.lm(uscrime,lm_model_1,m=4)
# Let us calculate the Rsquared error 
sse_model1<-95123*nrow(uscrime)
sst_model1<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq_model1<-1-sse_model1/sst_model1
rsq_model1


# Now the predict value is better than the previous . It is in the range.

# This shows that removing the variables that are not necessary actually reduces over fitting of the data.

# Let us analyse the results of the summary of lm_model_1

summary(lm_model_1)
# The Summary shows that if we Threshold(P_value) = 0.1, then factors above the threshold value 
# if removed might give us a better regression model 
# SO,LF,M.F,POP,NW,U1,Wealth,Time are above the threshold and can be removed

# Also let us consider the variance inflation factors
vif(lm_model_1)

# based on this -We can remove Wealth,Ineq,U1,PO1,SO,U2,NW

#As Wealth,U1, SO,Nw are common in both lists , let us  remove those and check the regression model.



lm_model_2 =update(lm_model_1,~.-Wealth-U1-So-NW)


#Predict the crime rate for the data point
predict_model_2 <-predict(lm_model_2,testpt)
predict_model_2
range(uscrime$Crime)
plot(lm_model_2)

lm_model_2_cv<-cv.lm(uscrime,lm_model_2,m=4)
# Let us calculate the Rsquared error 
sse_model2<-69838*nrow(uscrime)
sst_model2<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq_model2<-sse_model2/sst_model2
rsq_model2
# Let us analyse the results of the summary of lm_model_2

summary(lm_model_2)

# Also let us consider the variance inflation factors
vif(lm_model_2)


# As we see after removing the factors , the R-squared is little reduced.

#Let is further remove "LF,M.F,Pop,Time"

lm_model_3 =update(lm_model_2,~.-LF-M.F-Pop-Time)


#Predict the crime rate for the data point
predict_model_3 <-predict(lm_model_3,testpt)
predict_model_3


range(uscrime$Crime)
plot(lm_model_3)
# Let us analyse the results of the summary of lm_model_1
lm_model_3_cv<-cv.lm(uscrime,lm_model_3,m=4)
# Let us calculate the Rsquared error 
sse_model3<-48203*nrow(uscrime)
sst_model3<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq_model3<-1-sse_model3/sst_model3
rsq_model3
summary(lm_model_3)

# Also let us consider the variance inflation factors
vif(lm_model_3)


#Let us further remove Ineq and see the results
lm_model_4 =update(lm_model_3,~.-Ineq)


#Predict the crime rate for the data point
predict_model_4 <-predict(lm_model_4,testpt)
predict_model_4
range(uscrime$Crime)
plot(lm_model_4)
# Let us analyse the results of the summary of lm_model_1
lm_model_4_cv<-cv.lm(uscrime,lm_model_4,m=4)
# Let us calculate the Rsquared error 
sse_model4<-76417*nrow(uscrime)
sst_model4<-sum((uscrime$Crime-mean(uscrime$Crime))^2)
rsq_model4<-sse_model4/sst_model4
rsq_model4
summary(lm_model_4)



