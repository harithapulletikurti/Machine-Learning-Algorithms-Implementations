
#Install Packages
# warnings will be disabled
options(warn=-1)
rm(list = ls())
library(IRkernel)
library(forecast)
library(ggplot2)
library(survMisc)
library(tidyverse)
library(reshape)
library(ggfortify)
library(lubridate)


data <- read.table("temps.txt",stringsAsFactors = FALSE, header=TRUE)
head(data[1:4,])

#convert the data into time series

data_vec <- as.vector(unlist(data[,2:21]))
data_ts <- ts(data_vec, start = 1996, frequency = 123)

plot(decompose(data_ts))

#Run single/double/tripe exponential smoothing
#"The additive model is most appropriate if the magnitude of the seasonal
#fluctuations or the variation around the trend-cycle does not vary with 
#the level of the time series. When the variation in the seasonal pattern,
#or the variation around the trend-cycle, apprears to be proportional to the 
#level of the time series, then a multiplicative model is more appropriate.

set.seed(1)
#Holtwinter: using additive model
HW_Additive<- HoltWinters(data_ts,seasonal="additive")
summary(HW_Additive)
cat("HoltsWinter Additive method Results:\n\tBaseline factor alpha:", HW_Additive$alpha,"\n\tTrend factor beta:",HW_Additive$beta,"\n\tSeasonal factor gamma:",HW_Additive$gamma,"\n\tSum of Squared Errors:", HW_Additive$SSE,"\n")
par(mfrow=c(1,2))
plot(fitted(HW_Additive))

HW_Multiplicative <- HoltWinters(data_ts,alpha=NULL,beta=NULL,gamma=NULL,seasonal = "multiplicative")
cat("HoltsWinter Multiplicative method Results:\n\tBaseline factor alpha:", HW_Multiplicative$alpha,"\n\tTrend factor beta:",HW_Multiplicative$beta,"\n\tSeasonal factor gamma:",HW_Multiplicative$gamma,"\n\tSum of Squared Errors:", HW_Multiplicative$SSE,"\n")
summary(HW_Multiplicative)

par(mfrow=c(1,2))
plot(fitted(HW_Multiplicative))

HW_Multiplicative$fitted

#Not sure why exactly the trend component is the same for every time period,
#but it might be an average overall individual trend components.

#Going to look at the seasonal factors.


HW_M_seasonalfactor <-matrix(HW_Multiplicative$fitted[,4],nrow=123)
head(HW_M_seasonalfactor)


HW_xhat <- matrix(HW_Multiplicative$fitted[,1],nrow=123)
rownames(HW_xhat) <-data[,1]
columns = colnames(data[,-2])
colnames(HW_xhat) <-columns[-1]


head(HW_xhat)






S= c()
std=c()
DetectedDecreaseIndex = c()
DetectedDecrease = c()
S[0] = 0
total = 0
#finding a better T
for(j in 1:ncol(HW_xhat))
{
  std[j]= sd(HW_xhat[,j])
  total = total + std[j]
}
t= (total/ncol(HW_xhat))*3
t
# Let c= 5 and T = 20
# WE need to find when the temperature decreases so the summer ends.
t=24
C=5

for(j in 2:ncol(HW_xhat))
{
 
  for(i in 1:nrow(HW_xhat))
  {
    S[i] = max(0,S[i-1]+(mean(HW_xhat[,j])-HW_xhat[i,j] - C))
    if(S[i]>t)
    {
      DetectedDecreaseIndex[j-1] = i
      DetectedDecrease[j-1]=S[i]
      break
    }
  }
}

rows=rownames(HW_xhat)
cusum_year = colnames(HW_xhat)
cusum_decrease_date = c()
cusum_c = c()
cusum_t =c()
cusum_st = c()
cusum_dt=c()
temp=c()
for(k in 1:length(DetectedDecreaseIndex))
{
  
  cusum_decrease_date[k] = HW_xhat[DetectedDecreaseIndex[k],1]
  cusum_st[k]=DetectedDecrease[k]
  cusum_c[k] = C
  cusum_t[k] = t
  cusum_dt[k]=rows[DetectedDecreaseIndex[k]]
  temp[k]=HW_xhat[DetectedDecreaseIndex[k],k]
}
 

matrix.c = cbind(cusum_year,cusum_dt,temp,cusum_st,cusum_c,cusum_t)
colnames(matrix.c) = c("Year","Date","Ending Temperature", "Cusum S(t)", "C","Threshold")
matrix.c = as.table(matrix.c)
matrix.c
gg_plot(cusum_st,cusum_year)



S= c()
std=c()
DetectedDecreaseIndex = c()
DetectedDecrease = c()
S[0] = 0
total = 0
#finding a better T
for(j in 2:ncol(data))
{
  std[j]= sd(data[,j])
  total = total + std[j]
}
t= (total/ncol(data))*3
t
# Let c= 5 and T = 20
# WE need to find when the temperature decreases so the summer ends.
t=24
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

rows=rownames(data)
cusum_year = colnames(data)
cusum_decrease_date = c()
cusum_c = c()
cusum_t =c()
cusum_st = c()
cusum_dt=c()
temp=c()
for(k in 1:length(DetectedDecreaseIndex))
{
  
  cusum_decrease_date[k] = HW_xhat[DetectedDecreaseIndex[k],1]
  cusum_st[k]=DetectedDecrease[k]
  cusum_c[k] = C
  cusum_t[k] = t
  cusum_dt[k]=rows[DetectedDecreaseIndex[k]]
  temp[k]=HW_xhat[DetectedDecreaseIndex[k],k]
}


matrix.c = cbind(cusum_year,cusum_dt,temp,cusum_st,cusum_c,cusum_t)
colnames(matrix.c) = c("Year","Date","Ending Temperature", "Cusum S(t)", "C","Threshold")
matrix.c = as.table(matrix.c)
matrix.c


//Original data test

rm(list = ls())
data <- read.delim("temps.txt", header=T)
head(data)
tail(data)



S= c()
DetectedDecreaseIndex = c()
DetectedDecrease = c()
S[0] = 0

total = 0
#finding a better T
for(j in 2:ncol(data))
{
  std[j]= sd(data[,j])
  total = total + std[j]
}
t= (total/ncol(data))*3
t

# Let c= 5 and T = 20
# WE need to find when the temperrature decreases so the summer ends.
t=24
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
temp=c()
for(k in 1:length(DetectedDecreaseIndex))
{
  cusum_decrease_date[k] = data[DetectedDecreaseIndex[k],1]
  cusum_st[k]=DetectedDecrease[k]
  cusum_c[k] = C
  cusum_t[k] = t
  temp[k]=data[DetectedDecreaseIndex[k],k]
  
}
cusum_year 
cusum_decrease_date 
cusum_c
cusum_t 
cusum_st 

matrix.c = cbind(cusum_year,cusum_decrease_date,temp,cusum_st,cusum_c,cusum_t)
colnames(matrix.c) = c("Year","End of Summer","temp", "Cusum S(t)", "C","Threshold")
matrix.c = as.table(matrix.c)
matrix.c