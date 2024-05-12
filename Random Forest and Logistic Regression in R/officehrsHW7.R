rm(list = ls())
uscrime <- read.delim("uscrime.txt")
 
#Fit  a regression tree function to the crime data. Note that deviance 
# is a quality of fit statistic that is a generalization of the sum of
#squared residuals. Also,check out the function documentation to learn
#more about how the function makes splits in the tree.

# Regression Tree Model
library(tree)
uscrime_tree <- tree(Crime~., data = uscrime)
summary(uscrime_tree)

#visualize the regression tree, see how many points are in each leaf,
#and see which leaf each point is in.

plot(uscrime_tree)
text(uscrime_tree)
uscrime_tree$frame
uscrime_tree$where

#Manually compute R2. Is this a good measure of the quality of fit?
#Notice that we can only use averages of each leaf to make 
#predictions.

yhat_tree <- predict(uscrime_tree)
plot(yhat_tree,uscrime$Crime)

#Examine training and cv deviance for different tree sizes.
#what does this indicate about the quality of fit of our model?
#Should we prune some branches?

prune.tree(uscrime_tree)$size
prune.tree(uscrime_tree)$dev
set.seed(42)
cv.tree(object = uscrime_tree, FUN = prune.tree)

# we can do cross validation.

#Example of manaully pruning a tree in which we choose to onlu have 4 leaves

uscrime_tree_prune4 <- prune.tree(uscrime_tree,best = 4)
plot(uscrime_tree_prune4)
text(uscrime_tree_prune4)
summary(uscrime_tree_prune4)
crimeTree4Predict <- predict(uscrime_tree_prune4, data = uscrime[,1:15])
RSSofTree4 <- sum((crimeTree4Predict - uscrime[,16])^2)
TSS <- sum((uscrime[,16] - mean(uscrime[,16]))^2)
R2ofTree4 <- 1 - RSSofTree4/TSS
R2ofTree4

uscrime_tree_prune5 <- prune.tree(uscrime_tree,best = 5)
plot(uscrime_tree_prune5)
text(uscrime_tree_prune5)
summary(uscrime_tree_prune5)
crimeTree5Predict <- predict(uscrime_tree_prune5, data = uscrime[,1:15])
RSSofTree5 <- sum((crimeTree5Predict - uscrime[,16])^2)
R2ofTree5 <- 1 - RSSofTree5 /TSS
R2ofTree5

uscrime_tree_prune6 <- prune.tree(uscrime_tree,best = 6)
plot(uscrime_tree_prune6)
text(uscrime_tree_prune6)
summary(uscrime_tree_prune6)
crimeTree6Predict <- predict(uscrime_tree_prune6, data = uscrime[,1:15])
RSSofTree6 <- sum((crimeTree6Predict - uscrime[,16])^2)
R2ofTree6 <- 1 - RSSofTree6 /TSS
R2ofTree6

#since you are being asked to build a regression tree, you should
#at least attempt to build a regression model on one of the 
#leaves instead of just taking the average among the points in 
#the leaf for predictions. Will there be overfitting if we try to 
# build regression models for each of the original 7 leaves?

#Might want to consider looking at different numbers of branches 
# and/or different splitting criteria (look at the split argument).
#Can use the rpart() function from the rpart library very similarly.

#### Q10.1b###
# Random Forest

#rm(list = ls())
#uscrime <- read.table("uscrime.txt", stringsAsFactors =  FALSE, header = TRUE)

#Grow the random trees and set the number of predictors that you want
#to consider at each split of the tree(numpred). A good recommendation 
#for numpred is 1+log(n) or n/3 where n is the number of predictors.

library(randomForest)
set.seed(42)
num_pred <- 4
uscrime_rf <- randomForest(Crime~.,data = uscrime,mtry = num_pred,importance = TRUE , ntree = 500)
uscrime_rf

crime_rf_predict <- predict(uscrime_rf, data=uscrime[,-16])
RSS <- sum((crime_rf_predict - uscrime[,16])^2)
R2 <- 1 - RSS/TSS
R2

num_pred5 <- 5
uscrime_rf5 <- randomForest(Crime~.,data = uscrime,mtry = num_pred5,importance = TRUE , ntree = 500)
uscrime_rf5

crime_rf_predict5 <- predict(uscrime_rf5, data=uscrime[,-16])
RSS5 <- sum((crime_rf_predict5 - uscrime[,16])^2)
R2_pre5 <- 1 - RSS5/TSS
R2_pre5

#consider computing CV R^2 of this model. If you compare it to the 
#training R^2, what can we conclude about this model?


#remember that random forest can give you a model with better 
#of quality of fit, but it is a black model. we can use the importance
#function to see what are the most important factors for the 
#prediction are . %IncMSE is the amount that the MSE of predictions increases
# if the variable is randomly chosen instead of using its actual value.
#IncNodePurity measures how much splitting on it improves the similarity 
# of the data points in each leaf.

importance(uscrime_rf)

##########Q10.3########
set.seed(10)

rm(list = ls())
germancredit <- read.table("germancredit.txt",header = FALSE)
str(germancredit)

# Notice that we have many categorical predictors.

#Make the response variable binary in terms of 0 and 1.
germancredit$V21[germancredit$V21==1] <- 0
germancredit$V21[germancredit$V21==2] <- 1

head(germancredit)




#split the data into training and testing sets.
germancredit_train <- germancredit[1:800,]
germancredit_test <- germancredit[801:1000,]

#create a logistic regression model
germancredit_model = glm(V21~., family=binomial(link = "logit"),
                          data=germancredit_train)

summary(germancredit_model)

#consider doing some type of variable selection even though this has not
#been covered in the lectures yet. Also, notice how glm() implicitly
#creates dummy binary variables for each of the categorical variables.
#This is the correct way to do regression with categorical variables.
#however, if you want to do variable selection with these many dummy
#variables, you must re-define your categorical variables either manually
# or with an R function.

yhat<-predict(germancredit_model,germancredit_test[,-21],type= "response")
table(germancredit_test$V21, round(yhat))
#Important to use type = "response" here because without this
# we are given predictions of log-odds in the default case.

#"round" your the yhat to get binary predictions from which 
#you can compute an accuracy (classification rate). You may  want
#to try out differnt thresholds for rounding. You can also use AUC to 
#estimate the quality of fit.

library(pROC)


roc(germancredit_test$V21,round(yhat))
#Look at different threshold probability values and then compute the 
#cost that corresponds to each threshold.

thresh <- 0.8
yhat_thresh <- as.integer(yhat > thresh)
conf_matrix <- as.matrix(table(yhat_thresh,germancredit_test$V21))
conf_matrix

accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(conf_matrix[1,1]+conf_matrix[1,2]+conf_matrix[2,1]+conf_matrix[2,2])
accuracy

specificity <- (conf_matrix[1,1])/(conf_matrix[1,1]+conf_matrix[2,1])
specificity

thresh <- 0.7
yhat_thresh <- as.integer(yhat > thresh)
conf_matrix <- as.matrix(table(yhat_thresh,germancredit_test$V21))
conf_matrix
accuracy <- (conf_matrix[1,1]+conf_matrix[2,2])/(conf_matrix[1,1]+conf_matrix[1,2]+conf_matrix[2,1]+conf_matrix[2,2])
accuracy

specificity <- (conf_matrix[1,1])/(conf_matrix[1,1]+conf_matrix[2,1])
specificity

#(Cost computation from the confusion matrix goes here)
