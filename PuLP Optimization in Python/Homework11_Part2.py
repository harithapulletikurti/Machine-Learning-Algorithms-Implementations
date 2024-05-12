#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# Part -2 of Homework 11.
#If you want to see what a more full-sized problem would look like,
#try solving your models for the file diet_large.xls, which is a low-cholesterol 
#diet model (rather than minimizing cost, the goal is to minimize cholesterol intake).
#I don’t know anyone who’d want to eat this diet – the optimal solution includes dried 
#chrysanthemum garland, raw beluga whale flipper, freeze-dried parsley, etc. 
#– which shows why it’s necessary to add additional constraints beyond the
#basic ones we saw in the video!
#	[Note: there are many optimal solutions, 
#all with zero cholesterol, so you might get a different one.  
#It probably won’t be much more appetizing than mine.]

#import PuLP and pandas modules

from pulp import *
import pandas as pd
import numpy as np


# In[3]:


#Read the diet
data = pd.read_excel("C:\\Haritha\\GA-Tech Courses\\Sem 1 - ISYE 6501 - Intro to Analyics Modeling\\Homeworks\\Homework 11\\Homework 11 Solutions\\Data\\diet_large.xls", skiprows = 1, header = 0) 


# In[5]:



df = data[0:7146] 
df = df.values.tolist() # Convert dataframe to list


# In[7]:


# column headers (nutrient names are in columns 3-13; Excel calls them D-N)
Nutrients = list(data.columns.values) 
 # don't count the food name column
Nutrientcount = len(Nutrients) - 1


# In[10]:


for i in range(0,7146):
    for j in range(1,Nutrientcount):
        if (df[i][j]!=df[i][j]):
            df[i][j] = 0


# In[11]:


#minimum and maximum Nutrient Values
minimumVal = data[7147:7148].values.tolist() 
maximumVal = data[7149:7151].values.tolist() 


# In[12]:


# Initialize variables
foods = [i[0] for i in df] 


# In[13]:



cost = dict([(i[0], float(i[Nutrients.index('Cholesterol')])) for i in df]) 


# In[14]:


#Append the Nutrient Vals
N = []
for i in range(0,Nutrientcount):
    N.append(dict([(j[0], float(j[i+1])) for j in df])) 


# In[16]:


#Initialize Optimization Problem to minimize the cost.

OptimizationProblem = LpProblem('Diet_Optimization', LpMinimize) 


# In[17]:


#Initialize variables for Optimization
xi = LpVariable.dicts("Foods", foods, 0)


# In[19]:


# Constraints for each nutrient

# Add the minimum and maximum nutrient constraint
# only write a constraint if upper and lower bounds exist
for i in range(0,Nutrientcount): 
    if (not np.isnan(minimumVal[0][i+1])) and (not np.isnan(maximumVal[0][i+1])): 
        print("Constraint for " + Nutrients[i+1])
        OptimizationProblem += lpSum([N[i][j] * xi[j] for j in foods]) >= minimumVal[0][i+1], 'min nutrient ' + Nutrients[i+1]
        OptimizationProblem += lpSum([N[i][j] * xi[j] for j in foods]) <= maximumVal[0][i+1], 'max nutrient ' + Nutrients[i+1]
          


# In[20]:


#  Objective Function
#Total Cost
OptimizationProblem += lpSum([cost[i] * xi[i] for i in foods])


# In[21]:


#Solve the problem
OptimizationProblem.solve()


# In[24]:


#Print the output
for k in OptimizationProblem.variables():
    if k.varValue>0:
        print(str(k.varValue) + " units of " + str(k).replace('Foods_',''))
print()
print("Total Cholestrol = %f" %value(OptimizationProblem.objective))


# In[ ]:




