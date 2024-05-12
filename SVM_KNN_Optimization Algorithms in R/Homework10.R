rm(list=ls())
cancerdata <- read.table("breast-cancer-wisconsin.data.txt",stringsAsFactors = FALSE,header= FALSE,
                         na.strings = "?", sep=",")

# give meaningful names to the data 

colnames(cancerdata)<-c("ID", "Clump_Thickness", "Cell_Size",
                        "Cell_Shape", "Marginal_Adhesion", "Single_Epith_Cell_Size", 
                        "Bare_Nuclei",  "Bland_Chromatin","Normal_Nucleoli", "Mitoses", "Class")

cancerdata$Class <- as.factor(cancerdata$Class)
levels(cancerdata$Class) <-c(0,1)
#Find the missing data
head(cancerdata)
summary(cancerdata)
# Bare Nuclie column has 16 missing values

#Divide the data into training and testing sets
split_data <- function(cancerdata)
{
  set.seed(499)
  cancerdata <- subset(cancerdata,select=-c(ID))
  samplesize <- floor(0.75 * nrow(cancerdata))
  Train_indices <- sample(seq_len(nrow(cancerdata)),size = samplesize)
  trainingdata <- cancerdata[Train_indices,]
  testingdata <- cancerdata[-Train_indices,]
  
  return(list("Training" = trainingdata, "Test" = testingdata))
}

#Function to predict the best accuracy using
#K nearest neighbour fuction which takes input as Training set and Test Data set and list of k values.
library(caret)
my_knn_func <- function(TrainingSet, TestSet,list_of_K)
{
  Best_Accuracy = 0; Best_K = 0
  
  for(k_value in list_of_K)
  {
    knnmodel <- knn3(Class~., data = TrainingSet, k = k_value)
    prediction_vals <- predict(knnmodel, TestSet[0:(length(TestSet)-1)], type= "class")
    prediction_accuracy <- round((sum(prediction_vals == TestSet$Class)/length(TestSet$Class)),digits=3)
    
    if(prediction_accuracy > Best_Accuracy)
    {
      Best_K <- k_value
      Best_Accuracy <- prediction_accuracy
    }
  }
  return(list("Best_K" = Best_K, "Best_Accuracy"=Best_Accuracy))
}

#Case 1: Remove all the missing rows and run the knn fuction to find the accuracy
list_of_k <- seq(1,15) 
drop_missing <- na.omit(cancerdata)
GetSplitData <- split_data(drop_missing)
GetK_And_Accuracy <- my_knn_func(GetSplitData$Training,GetSplitData$Test,list_of_k )
print("Results after dropping Missing rows:")
GetK_And_Accuracy$Best_K
GetK_And_Accuracy$Best_Accuracy

#14.1 1.	Use the mean/mode imputation method to impute values for the missing data.

# Impute the mean of the Bare_Nuclei for the missing values 
cancerdata_with_mean <- cancerdata
cancerdata_with_mean$Bare_Nuclei[is.na(cancerdata_with_mean$Bare_Nuclei)]<-mean(cancerdata_with_mean$Bare_Nuclei,na.rm=TRUE)
rm(GetSplitData)
GetSplitData <- split_data(cancerdata_with_mean)
GetK_And_Accuracy <- my_knn_func(GetSplitData$Training,GetSplitData$Test,list_of_k )
print("Results after imputing mean:")
GetK_And_Accuracy$Best_K
GetK_And_Accuracy$Best_Accuracy

# Impute the mode of Bare Nuclie
cancerdata_with_mode <- cancerdata
cancerdata_with_mode$Bare_Nuclei[is.na(cancerdata_with_mode$Bare_Nuclei)]<-mode(cancerdata_with_mode$Bare_Nuclei)
rm(GetSplitData)
GetSplitData <- split_data(cancerdata_with_mode)
#GetK_And_Accuracy <- my_knn_func(GetSplitData$Training,GetSplitData$Test,list_of_k )
print("Results after imputing mode:")
GetK_And_Accuracy$Best_K
GetK_And_Accuracy$Best_Accuracy
99.3

#Use Regression to impute the values for the missing data
library(mice)
impute <- mice(cancerdata,method="norm.predict",m=1)
data_impute <- complete(impute)

rm(GetSplitData)
GetSplitData <- split_data(data_impute)
GetK_And_Accuracy <- my_knn_func(GetSplitData$Training,GetSplitData$Test,list_of_k )
print("Results after imputing Regression:")
GetK_And_Accuracy$Best_K
GetK_And_Accuracy$Best_Accuracy


#use Regression with perturbation to impute the values for the missing data
# and run knn

impute <- mice(cancerdata,method="norm.nob",m=1)
data_impute <- complete(impute)

rm(GetSplitData)
GetSplitData <- split_data(data_impute)
GetK_And_Accuracy <- my_knn_func(GetSplitData$Training,GetSplitData$Test,list_of_k )
print("Results after imputing Regression:")
GetK_And_Accuracy$Best_K
GetK_And_Accuracy$Best_Accuracy

