#Question 5.1

#Using crime data from the file uscrime.txt (http://www.statsci.org/data/general/uscrime.txt,
#description at http://www.statsci.org/data/general/uscrime.html), test to see whether there
#are any outliers in the last column (number of crimes per 100,000 people). 
#Use the grubbs.test function in the outliers package in R.

# Start with a clear environment
rm(list = ls())

setwd("C:\\Users\\harit\\OneDrive\\Documents\\GA-Tech Courses\\Sem 1 - ISYE 6501 - Intro to Analyics Modeling\\Homeworks\\Homework 3 Question\\Homework 3 Solution")

#load the libraries
library(knitr)
library(stringr)
library(outliers)
#load the crime data with headers
crime_data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

head(crime_data)
tail(crime_data)
dim(crime_data)

crime <- crime_data[,"Crime"]
plot(crime)
shapiro.test(crime)
qqnorm(crime)
#The qq norm show that the data is 90% of the data is almost normal

set.seed(1) 
#Using Grubbs Test find the mininum and maximum outliers using type = 11
#type = 11 gives 2 outliers one- min and one-max  
# I am not using type = 20 as it accepts less data around 30 data values.
# As we have more data, type = 20 gives error. So choosing type = 11
outlier_results<-grubbs.test(crime_data[,16],type=11)
outlier_results

#Inference: 343 and 1993 are outliers. Let us plot them in boxplot and visualize
values<- as.numeric(str_extract_all(outlier_results$alternative, "[0-9]+")[[1]])
Outliers_data<- subset(crime_data, Crime %in% values)
boxplot(crime_data[,16],
        ylab="Crime Value")
points(Outliers_data[,ncol(Outliers_data)], pch=19,col='blue')
title(main="Crime Outliers")

#Inference : Based on the box plot, the blue dot is 1993. Also looks like the 1969 is also an outlier.
#Since grubbs test only gets the min and max outliers (only 2) 1969 is not listed in the test result
# but it is shown in the boxplot next to 1993. Also 342(min value) is returned as outlier in the gurbbs.test.

#Question 6.1

#Describe a situation or problem from your job, everyday life, current events, etc.,
#for which a Change Detection model would be appropriate. Applying the CUSUM technique, 
#how would you choose the critical value and the threshold?
   
#The Cusum Apprach can be used in Production Assembly lines where a lot of Robots are involved in assembling the 
#parts of the units. A small change in the movement of the Robotic Holding Parts will make a huge impact on whether the
# assembly takes place successfully or the process crashes.
# Example : Consider a car manufacturing plant, where in each assembly line a different car part is being assembled by the robots.
# Suppose the robot hand has to drop an item onto the conveyor belt at an angle of 45 - 50 degrees and the item successfully lands at the place
#Say the angle is changed to 51 degrees instead of the allowed threshold of 50  degrees, the item  fails to land successfully.
# Using cusum approach on the everyday data collected of such processes, we can determine at what point the most failures happen and we will be
# able to correct the process to be more successful



#Question 6.2

#1.	Using July through October daily-high-temperature data for Atlanta for 1996 through 2015,
#use a CUSUM approach to identify when unofficial summer ends 
#(i.e., when the weather starts cooling off) each year.  You can get the data that you need 
#from the file temps.txt or online, for example at http://www.iweathernet.com/atlanta-weather-records
#or https://www.wunderground.com/history/airport/KFTY/2015/7/1/CustomHistory.html .  
#You can use R if you’d like, but it’s straightforward enough that an Excel spreadsheet
#can easily do the job too.

rm(list = ls())
data <- read.delim("temps.txt", header=T)
head(data)
tail(data)



S= c()
DetectedDecreaseIndex = c()
DetectedDecrease = c()
S[0] = 0

# Let c= 5 and T = 20
# WE need to find when the temperrature decreases so the summer ends.
t=20
C=5
for(j in 2:ncol(data))
{
 for(i in 1:nrow(data))
 {
    S[i] = max(0,S[i-1]+(mean(data[,j])-data[i,j] - C))
    if(S[i]>t)
    {
      DetectedDecreaseIndex[j-1] = i
      DetectedDecrease[j-1]=S[i]
      break
    }
  }
}
cusum_year = colnames(data[-1])
cusum_decrease_date = c()
cusum_c = c()
pl
cusum_t =c()
cusum_st = c()
for(k in 1:length(DetectedDecreaseIndex))
{
 cusum_decrease_date[k] = data[DetectedDecreaseIndex[k],1]
 cusum_st[k]=DetectedDecrease[k]
 cusum_c[k] = C
 cusum_t[k] = t


}
cusum_year 
cusum_decrease_date 
cusum_c
cusum_t 
cusum_st 

matrix.c = cbind(cusum_year,cusum_decrease_date,cusum_st,cusum_c,cusum_t)
colnames(matrix.c) = c("Year","End of Summer temp", "Cusum S(t)", "C","Threshold")
matrix.c = as.table(matrix.c)
matrix.c

#Inference : From the Year"1996 -2002" , summer ended around mid to end of September
#From Year "2003-2009" Summer ended around Mid October
#"From Year "2010-2011 and 2014-2015" Summer ended early again end of September
#For Years 2013 Summer ended very soon in mid August.

#Question(2) 
#	Use a CUSUM approach to make a judgment of whether Atlanta’s summer climate has gotten warmer 
#in that time (and if so, when).
# Start with a clear environment

rm(list = ls())
data <- read.delim("temps.txt", header=T)
head(data)
tail(data)

#model1996temp<-data[,1:2]
#model1996 <-model1996temp[,-1]

St= c()
DetectedIncreaseIndex = c()
DetectedIncrease = c()
St[0] = 0

# Let c= 5 and T = 20
# WE need to find when the temperature rises
t=10
C=5
for(j in 2:ncol(data))
{
  for(i in 1:nrow(data))
  {
    St[i] = max(0,St[i-1]+(data[i,j]- mean(data[,j]) - C))
    if(St[i]>t)
    {
      DetectedIncreaseIndex[j-1] = i
      DetectedIncrease[j-1]=St[i]
      break
    }
  }
}
cusum_year = colnames(data[-1])
cusum_increase_date = c()
cusum_c = c()
cusum_t =c()
cusum_st_inc = c()
for(k in 1:length(DetectedIncreaseIndex))
{
  cusum_increase_date[k] = data[DetectedIncreaseIndex[k],1]
  cusum_st_inc[k]=DetectedIncrease[k]
  cusum_c[k] = C
  cusum_t[k] = t
  
}
cusum_year 
cusum_increase_date 
cusum_c
cusum_t 
cusum_st_inc 

matrix.HighSummer = cbind(cusum_year,cusum_increase_date,cusum_st_inc,cusum_c,cusum_t)
colnames(matrix.HighSummer) = c("Year","Increased Summer", "Cusum S(t)", "C","Threshold")
matrix.HighSummer = as.table(matrix.HighSummer)
matrix.HighSummer


#Inference : 
#Using the CUSUM approach, I calculated the increase in Atlanta Temperatures.
#The most high summers are in July based on the above table.
#I have selected the Threshold = 10 and C=5.
#Note : When The Threshold is 20 or above, few year's St value is null. 
#So Threshold = 10 is choosen as ideal.
#