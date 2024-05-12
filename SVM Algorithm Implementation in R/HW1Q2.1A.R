
#start with a clean environment
rm(list=ls()) 

#Homework Assignment - 1 By Haritha Pulletikurti 

########################################################################################
# QUestion 2.2.                                                                        #
#Question 1.	Using the support vector machine function ksvm contained in the R package#
#kernlab,find a good classifier for this data. Show the equation of your classifier,  #
#and how well it classifies the data points in the full data set.                      #                    
#(Don’t worry about test/validation data yet; we’ll cover that topic soon.)            #
########################################################################################





#loading libaries
library(kernlab)
library(kknn)
library(e1071)
library(data.table)
#set the working directory
setwd("C:\\R Projects")

#make sure to reproduce the test results
set.seed(50)

#import the  credit card dataset with headers
credit_card_dataset <- read.delim("credit_card_data-headers.txt")


#Take a sample set of the first and last few rows of credit_card_dataset
head(credit_card_dataset)
tail(credit_card_dataset)
# Find the Optimum C Value and generate the optimum classifier for the the data.

# Ranges of C 
C_Ranges = c(0.1,1,10,100,1000,10000)
AccuracyResults_Based_On_Full_C_Range=list()
SVMmodel=list()
svmmodelprediction=list()
# Get the Model Predictions for all C's in the list ans store them for analysis
for(i in 1:length(C_Ranges))
{
  # call ksvm.  Vanilladot is a simple linear kernel.
  SVMmodel[[i]] <- ksvm(as.matrix(credit_card_dataset[,1:10]),as.factor(credit_card_dataset[,11]),type="C-svc",kernel="vanilladot",C=C_Ranges[[i]],scaled=TRUE)
  # see what the model predicts
  svmmodelprediction[[i]] <- predict(SVMmodel[[i]],credit_card_dataset[,1:10])
  
  # see what percentage of the model’s predictions match the actual classification
  AccuracyResults_Based_On_Full_C_Range[[i]]= sum(svmmodelprediction[[i]] == credit_card_dataset[,11]) / nrow(credit_card_dataset)*100
}

#Order of Accuracy from least to highest 
Max_AccuracyIndices = order(unlist(AccuracyResults_Based_On_Full_C_Range))


TopFiveCRanges=c(C_Ranges[Max_AccuracyIndices[length(Max_AccuracyIndices)]],C_Ranges[Max_AccuracyIndices[length(Max_AccuracyIndices)-1]],
                 C_Ranges[Max_AccuracyIndices[length(Max_AccuracyIndices)-2]],C_Ranges[Max_AccuracyIndices[length(Max_AccuracyIndices)-3]],
                 C_Ranges[Max_AccuracyIndices[length(Max_AccuracyIndices)-4]])
#Top Five C Ranges based on highest Accuracy Values
TopFiveCRanges 


#Analysis of Top Five C Ranges 
CPlotValues = c()
nSVPlotValues = c()
errorPlotValues = c()
accuracyplotvalues = c()
j=1
for(i in length(Max_AccuracyIndices):1)
{
  
  CPlotValues[j]=C_Ranges[Max_AccuracyIndices[i]]
  CPlotValues[j]
  SVMmodel[Max_AccuracyIndices[i]]
  svmmodelprediction[Max_AccuracyIndices[i]]
  accuracyplotvalues[j]= AccuracyResults_Based_On_Full_C_Range[Max_AccuracyIndices[i]]
  accuracyplotvalues[j]
  nSVPlotValues[j]=SVMmodel[Max_AccuracyIndices[i]][[1]]@nSV
  nSVPlotValues[j]
  errorPlotValues[j]=SVMmodel[Max_AccuracyIndices[i]][[1]]@error
  errorPlotValues[j]
  j=j+1
}
#Analysis and determining the Best C Values:
# C Ranges : Best to Least based on Accuracy of Prediction 
CRange = sprintf(CPlotValues,fmt='%#.2f')
CRange
#KSVM Model Accuracy Prediction Values from Best to Least
unlist(accuracyplotvalues)
#KSVM Model Error Prediction Values from Best to Least
errorPlotValues
#KSVM Model "Number of Support Vectors: Prediction Values from Best to Least
nSVPlotValues


plot(CRange, errorPlotValues, xlab="C-Ranges", ylab="Error Range" , main="Plot of C Ranges vs Errors" , col="red" , varwidth=T,horizontal=T)

plot(CRange, nSVPlotValues, xlab="C-Ranges", ylab="Number of Support Vectors" , main="Plot of C Ranges vs Number of Support Vectors" , col="red" , varwidth=T,horizontal=T)

# Based on the above analysis, 
# Top C Values based on Accuracy of Prodiction are : C=100,  C=10 and C=1 where accuracies are all 80.39144%
# Top Error Allowed of the Top C Values are : for all C= 100, 10, 1 is 0.1360856
# Top Number of Support Vectors(NSV) : C= 100 and NSV = 189 , C=10 and NSV = 190 , C=1 and NSV = 190.
# For More larger C Values like C=1000 and 10000 the NSV > 275 implying the margin is too large. So not optimal
# For C values 0.1 and the margin is very small. The new data points can be misclassified to a great extent.
# Hence the optimum value of C=100 and the second best choice would be C=10 based on the above Research.

BestSVMmodel <- ksvm(as.matrix(credit_card_dataset[,1:10]),as.factor(credit_card_dataset[,11]),type="C-svc",kernel="vanilladot",C=100,scaled=TRUE)

BestSVMmodel


# calculate the cofficients a1…am of the  model predicted support vectors
a1ToamCoeff<- colSums(BestSVMmodel@xmatrix[[1]] * BestSVMmodel@coef[[1]])
a1ToamCoeff
# calculate a0
a0Intercept <- BestSVMmodel@b
a0Intercept

Bestsvmmodelprediction <- predict(BestSVMmodel,credit_card_dataset[,1:10])
Bestsvmmodelprediction

# see what percentage of the model’s predictions match the actual classification
AccuracyResults= sum(Bestsvmmodelprediction == credit_card_dataset[,11]) / nrow(credit_card_dataset)*100

AccuracyResults

##########################################################################################################
#Question 2.	You are welcome, but not required, to try other (nonlinear) kernels as well; we’re not covering them #
#in this course, but they can sometimes be useful and might provide better predictions than vanilladot.###
###########################################################################################################




PolyDotKernelSVMmodel <- ksvm(as.matrix(credit_card_dataset[,1:10]),as.factor(credit_card_dataset[,11]),type="C-svc",kernel="polydot",C=100,scaled=TRUE)

PolyDotKernelSVMmodel


# calculate the cofficients a1…am of the  model predicted support vectors
a1ToamCoeffofPolydotKernel<- colSums(PolyDotKernelSVMmodel@xmatrix[[1]] * PolyDotKernelSVMmodel@coef[[1]])
a1ToamCoeffofPolydotKernel
# calculate a0
a0InterceptofPolydotKernel <- PolyDotKernelSVMmodel@b
a0InterceptofPolydotKernel

PolyDotKernelsvmmodelprediction <- predict(PolyDotKernelSVMmodel,credit_card_dataset[,1:10])
PolyDotKernelsvmmodelprediction

# see what percentage of the model’s predictions match the actual classification
AccuracyResultsforPolyDotKernel= sum(PolyDotKernelsvmmodelprediction == credit_card_dataset[,11]) / nrow(credit_card_dataset)*100

AccuracyResultsforPolyDotKernel


#Inference: The Polydot Kernel gives 86.39144 % of accuracy rate with 0.136086 Training error, Number of Support Vectors = 190 for C=100.




###########################################################################################################
##################Question : 2.2.3 ########################################################################
#Question 3.	Using the k-nearest-neighbors classification function kknn contained in the R kknn package,##
#suggest a good value of k, and show how well it classifies that data points in the full data set.#########
#Don’t forget to scale the data (scale=TRUE in kknn).#####################################################
##########################################################################################################

#Function(Knearest) takes a value for k and predicts which class the new data point belongs to.

classify_Based_on_kknn = function(Knearest){
  #Create n zero predictions
  predictions<- rep(0,(nrow(credit_card_dataset))) 
  
  for (i in 1:nrow(credit_card_dataset)){
    #Run the kknn function and ensure it doesn't use i itself 
    KNNModel=kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,credit_card_dataset[-i,],
                  credit_card_dataset[i,],k=Knearest, distance = 2,kernel = "optimal",scale = TRUE) 
    predictions[i] <- as.integer(fitted(KNNModel)+0.5)
  }
  
  #Check the accuracy of the prediction.
  account = sum(predictions == credit_card_dataset[,11]) / nrow(credit_card_dataset)
  return(account)
}

# Create 85 zeroed vector for accuracy test.
RunTestVectors <- rep(0,85) 
#Run the K value (Knearest_Value) from 1 to 85 .
for (Knearest_Value in 1:85){
  RunTestVectors[Knearest_Value] = classify_Based_on_kknn(Knearest_Value) 
}

#see accuracy as percentage
KNNAccuracyAsPercentage <- as.matrix(RunTestVectors * 100)

#print out knn values and percentage of accuracy
KNNAccuracyAsPercentage 


knnValue <- c(1:85)

#Plot the  KKN accuracies as Percentage per Kth Nearest Neighbour value
plot(knnValue,KNNAccuracyAsPercentage)

#Maximum Percentage
max(KNNAccuracyAsPercentage)

#Inference

#The  highest accuracy  of 85.321110 is at k = 15. So, the classifier is optimal at k = 15.
#The  highest accuracy  of 85.321110 is at k = 12. So, the classifier is also optimal at k = 12.
# The accuracy value seems to decrease as K value increases more than 15.
# Hence the optimal value of k = 12 for this data set.