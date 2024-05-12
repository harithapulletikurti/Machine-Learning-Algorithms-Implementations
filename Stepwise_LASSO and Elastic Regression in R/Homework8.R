rm(list = ls())
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

# perform backward elemination, trace argument can be used to supress the output

model_back <- lm(Crime~., data = TrainingData)
step(model_back,direction ="backward")


exp((376.50-375.54)/2)
#Though this has the lowest AIC there is still some uncertainity that it could be even better.
# Analysis : low likely is the best model better than the better model using aic

#perform forward selection
model_forward <-lm(Crime~1,data = TrainingData)
step(model_forward, 
     scope = formula(lm(Crime~.,data=TrainingData)),
     direction = "forward")

exp((376.35-376.18)/2)

#Analysis:
  
model_both <- lm(Crime~.,data = TrainingData)
step(model_both,
     scope = list(lower = formula(lm(Crime~1,data=TrainingData)),
                  upper = formula(lm(Crime~.,data = TrainingData))),
                  direction = "both")
     


model_final <- lm(Crime~Ed + Po1 + M.F +Pop + U1 +U2 + Ineq + Time, data = TrainingData)
Summary(model_final)
plot(model_final)

#lasso
library(glmnet)
set.seed(42)
model_lasso <- cv.gmlnet(x=as.matrix(uscrime[,-16]),
                         y = as.matrix(uscrime[,16]),
                         alpha = 1 ,
                         nfolds = 8,
                         nlambda = 20,
                         type.measure = "mse",
                         family ="guassian",
                         standardize = TRUE)

model_lasso
plot(model_lasso)
model_lasso$lambda.min
cbind(model_lasso$lambda, model_lasso$cvm , model_lasso$nzero)
coef(model_lasso,s=model_lasso$lambda.min)



