# Stepwise selection

rm(list = ls())
set.seed(82)
uscrime<- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

uscrime[1:3,]

# Scale the data
Scaleduscrime <- as.data.frame(scale(uscrime[,c(1,3:15)]))
Scaleduscrime <- cbind(uscrime[,2],Scaleduscrime,uscrime[,16])
colnames(Scaleduscrime)[1] <- "So"
colnames(Scaleduscrime)[16] <- "Crime"

Scaleduscrime[1:3,]


# Split the data into Training and Test Datasets.
library(caret)
randomrows <-  createDataPartition(y=1:nrow(Scaleduscrime),p=0.7, list = FALSE)
TrainingData = Scaleduscrime[randomrows,]
TestData = Scaleduscrime[-randomrows,]
dim(TrainingData)
dim(TestData)



library(olsrr)
# Perform Forward Regression using p-values

model<-lm(Crime~.,data = TrainingData)

summary(model)

Forwardfit.p <- ols_step_forward_p(model,penter = 0.5)
Forwardfit.p

# Perform Forward Regression using aic
model<-lm(Crime~.,data = TrainingData)
Forwardfit.aic <-ols_step_forward_aic(model, details = TRUE)
Forwardfit.aic

plot(Forwardfit.aic)

#Analysis :
#backward elimination model using p values

model<-lm(Crime~.,data = TrainingData)
BackwardFit.p <- ols_step_backward_p(model, prem = 0.5)
BackwardFit.p

# Backward Elimination model using aic

BackwardFit.aic <- ols_step_backward_aic(model, details = TRUE)

BackwardFit.aic 

plot(BackwardFit.aic)

#Analysis :


# Stepwise Regression using both directions and p-vlaues
model<-lm(Crime~.,data = TrainingData)

StepwiseBothFit.p <- ols_step_both_p(model,pent = .05, prem = 0.5)
StepwiseBothFit.p

# Stepwise Regression using both directions and aic 

model = model<-lm(Crime~.,data = TrainingData)

StepwiseBothFit.aic<- ols_step_both_aic(model, details = TRUE)
StepwiseBothFit.aic

plot(StepwiseBothFit.aic)

#Analysis : All the three methods, Forward Regression, Backward Elimination and 
# Stepwise Regression in both Directions returned the model
# with lm(Crime~Po1 + Ineq+ Prob  + Wealth  + M + Ed + U2 ) using the Scaled Training 
# Data.

BestModelWithTrainingData<- lm(Crime~Po1 + Ineq  + Wealth  + M + Ed + U2 +Prob, data = TrainingData)
summary(BestModelWithTrainingData)


BestModelwithTestData<- lm(Crime~Po1 + Ineq  + Wealth  + M + Ed + U2 + Prob, data = TestData)
summary(BestModelwithTestData)

#Analysis: Compare Initial Model,
#Compare the Best Models given by all three methods
#Compare the Final train/test best model.
#Write about the R^2.




#b. Lasso Regression : 
  

library(glmnet)
set.seed(82)

# The cv part means we want to use Cross validation to 
#obtain the optimalvalues for Lambda.
model_lasso <- cv.glmnet(x=as.matrix(TrainingData[,-16]),
                         y = as.matrix(TrainingData[,16]),
                         alpha = 1 ,
                         nfolds = 8,
                         nlambda = 20,
                         type.measure = "mse",
                         family ="gaussian",
                         standardize = TRUE)
model_lasso.predicted<-predict(model_lasso,s=model_lasso$lambda.1se,newx=as.matrix(TestData[,-16]))
#Lambda.1se is the value of lambda,that resulted in the simplest model(model with few non zero parameters)
#and was within 1 standard error of the lambda that had the smallest sum.
model_lasso.predicted
mean((TestData[,16] - model_lasso.predicted)^2)

# Find the accuracy 
sse = sum((model_lasso.predicted - TestData[,16])^2)
totalSumofSquares = sum((TestData[,16]-mean(TestData[,16]))^2)
RSquared = 1- (sse/totalSumofSquares)
AdjustedRSqaured = RSquared - (1-RSquared)*15/(nrow(TestData)-15-1)
AdjustedRSqaured
RSquared



#Elastic.Net Regression 

# The cv part means we want to use Cross validation to 
#obtain the optimalvalues for Lambda.
model_elasticnet_alpha0.5 <- cv.glmnet(x=as.matrix(TrainingData[,-16]),
                         y = as.matrix(TrainingData[,16]),
                         alpha = 0.5 ,
                         nfolds = 8,
                         nlambda = 20,
                         type.measure = "mse",
                         family ="gaussian",
                         standardize = TRUE)
model_elasticnet_alpha0.5.predicted<-predict(model_elasticnet_alpha0.5,s=model_elasticnet_alpha0.5$lambda.1se,newx=as.matrix(TestData[,-16]))
#Lambda.1se is the value of lambda,that resulted in the simplest model(model with few non zero parameters)
#and was within 1 standard error of the lambda that had the smallest sum.
model_elasticnet_alpha0.5.predicted

# Find the accuracy 
sse = sum((model_elasticnet_alpha0.5.predicted - TestData[,16])^2)
totalSumofSquares = sum((TestData[,16]-mean(TestData[,16]))^2)
RSquared = 1- (sse/totalSumofSquares)
AdjustedRSqaured = RSquared - (1-RSquared)*15/(nrow(TestData)-15-1)
AdjustedRSqaured
RSquared

#Lets try more values of alpha
# We create the Elastic.NET fit using the cv.glmnet() function,
#which takes alpha values from 0.0,0.1,..1.0.
list_of_fits <- list()
for(i in 0:10)
{
  fit.name <- paste0("alpha",i/10)
  list_of_fits[[fit.name]] <- cv.glmnet(x=as.matrix(TrainingData[,-16]),
                                        y = as.matrix(TrainingData[,16]),
                                        alpha = i/10 ,
                                        nfolds = 8,
                                        type.measure = "mse",
                                        family ="gaussian",
                                        standardize = FALSE)
}

results <- data.frame()
# This for loop will give us the error values for each model from above.
for(i in 0:10)
{
  fit.name <- paste0("alpha",i/10)
  predicted <- predict(list_of_fits[[fit.name]],
                       s=list_of_fits[[fit.name]]$lambda.1se,newx=as.matrix(TestData[,-16]))
  
  # Find the accuracy 
  sse = sum((predicted - TestData[,16])^2)
  totalSumofSquares = sum((TestData[,16]-mean(TestData[,16]))^2)
  RSquared = 1- (sse/totalSumofSquares)
  temp <- data.frame(alpha=i/10, Rsqaured=RSquared, fit.name)
  results <- rbind(results, temp)
}
 
results 
  
