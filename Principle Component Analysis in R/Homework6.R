
rm(list = ls())
set.seed(1)

#Read the crime data
crimedata <- read.table("uscrime.txt",stringsAsFactors = FALSE, header = TRUE)
#Prinicipal component analysis on the crime data. The prcomp function takes
# all predictor columns except for the response coumn (Crime) on scaled data.
#
principalcomponents <- prcomp(crimedata[,-16],center=TRUE,scale=TRUE)

summary(principalcomponents)
# Analysis : The first 4 PC's are more important.
# Standard Deviation "square roots of the eigenvalues of the covariance/correlation matrix"
# rotation - "matric of eigen valued columns
library(DAAG)

# How many PCS to choose :
#If you divide each value by the total sum of eigenvalues
#prior to plotting, then your plot will show the fraction
#of total variance retained vs. number of eigenvalues. 

varianceofeachEigenColumn<-principalcomponents$sdev^2
 
proportionalvariance <- varianceofeachEigenColumn/sum(varianceofeachEigenColumn)
proportionalvariance

plot(proportionalvariance,xlab="Principal Component", ylab="Proportional Variance",
     ylim=c(0,1),type = "b")
# The plot shows that at after 5 Principle Component, there is a steady drop in the curve.
# So let us choose the first 5 Principle Components. Hence K = 5

k =5
# Create a datadrame with first 5 principe components and the reponse column
PCData= cbind(principalcomponents$x[,1:k],crimedata[,16])
PCData
LinearRegressionPCmodel <- lm(V6~.,data = as.data.frame(PCData))
summary(LinearRegressionPCmodel)


# We need to go back to the original coordinate system
# 1. We need to scale back the data
# xs = x-meanx/sigmax  => x = xs*sigmax + meanx
#ys = y-meany/sigmay => y = ys*sigmay + meany
# Intercept= Intercept(sc) - ScaledCoefficients (Meanx/sigmax) -ScaledCoefficients(Meany/Sigmay)

#Intercept
Interceptscaled <- LinearRegressionPCmodel$coefficients[1]
Interceptscaled
#Scaled Coefficents
scaledcoefs <-LinearRegressionPCmodel$coefficients[2:(k+1)]

# Rotate tge scaled data back. OriginalRotatationScaledData = RotationMatrix * Scaled data
OriginalRotatationScaledData <- principalcomponents$rotation[,1:k]%*%scaledcoefs

summary(OriginalRotatationScaledData)

mu <- sapply(crimedata[,1:15],mean)
sigma <-sapply(crimedata[,1:15],sd)

originalCoeff = OriginalRotatationScaledData/sigma
OriginalIntercept = Interceptscaled - sum(OriginalRotatationScaledData*mu/sigma)


original = as.matrix(crimedata[,1:15]) %*% originalCoeff+OriginalIntercept

# Find the accuracy 
sse = sum((original - crimedata[,16])^2)
totalSumofSquares = sum((crimedata[,16]-mean(crimedata[,16]))^2)
RSquared = 1- (sse/totalSumofSquares)
AdjustedRSqaured = RSquared - (1-RSquared)*k/(nrow(crimedata)-k-1)
AdjustedRSqaured

RSquared
AdjustedRSqaured

testpt <- data.frame(M = 14.0,So = 0,Ed = 10.0,Po1 = 12.0,
                     Po2 = 15.5,LF = 0.640, M.F = 94.0, Pop = 150,
                     NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,
                     Ineq = 20.1,Prob = 0.04,Time = 39.0)
#Predict the crime rate for the data point
# Replace PCA data into the test point
PCATestPoint <- data.frame(predict(principalcomponents,testpt))
predict_model <-predict(LinearRegressionPCmodel,PCATestPoint)
predict_model
