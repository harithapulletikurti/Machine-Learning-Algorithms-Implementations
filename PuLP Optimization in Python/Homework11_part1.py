#!/usr/bin/env python
# coding: utf-8
"""
Question 15.2

In the videos, we saw the “diet problem”. (The diet problem is one of the first large-scale
optimization problems to be studied in practice. Back in the 1930’s and 40’s, the Army wanted
to meet the nutritional requirements of its soldiers while minimizing the cost.)
In this homework you get to solve a diet problem 
with real data. The data is given in the file diet.xls. 

1.	Formulate an optimization model (a linear program) to find the cheapest diet that 
satisfies the maximum and minimum daily nutrition constraints, and solve it using PuLP.  
Turn in your code and the solution.
(The optimal solution should be a dietof air-popped popcorn, poached eggs, oranges
, raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)

2.	Please add to your model the following constraints (which might require adding more variables)
and solve the new model:

a.	If a food is selected, then a minimum of 1/10 serving must be chosen. 
    (Hint: now you will need two variables for each food i: whether it is chosen, 
    and how much is part of the diet. You’ll also need to write a constraint to link them.)
b.	Many people dislike celery and frozen broccoli. So at most one, but not both, 
     can be selected.
c.	To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must
     be selected. [If something is ambiguous (e.g., should bean-and-bacon soup be considered 
     meat?), just call it whatever you think is appropriate – I want you to learn how to write this 
     type of constraint, but I don’t really care whether we agree on how to classify foods!]

If you want to see what a more full-sized problem would look like, try solving your models for the
 file diet_large.xls, which is a low-cholesterol diet model (rather than minimizing cost, the goal is 
 to minimize cholesterol intake).  I don’t know anyone who’d want to eat this diet – the optimal 
 solution includes dried chrysanthemum garland, raw beluga whale flipper, freeze-dried parsley,
 etc. – which shows why it’s necessary to add additional constraints beyond the basic ones we saw 
 in the video!
	[Note: there are many optimal solutions, all with zero cholesterol, so you might get a
    different one.  It probably won’t be much more appetizing than mine.]
"""



# In[6]:


#load the libraries 
#  install pulp

from pulp import *
import pandas as pd


# In[8]:


#load the diet data
df = pd.read_excel(
open('C:\Haritha\GA-Tech Courses\Sem 1 - ISYE 6501 - Intro to Analyics Modeling\Homeworks\Homework 11\Homework 11 Solutions\Data\diet.xls',
    'rb'), sheet_name='Sheet1')


# In[12]:


# Print the data
df.head


# In[13]:


# Take only the first 65 rows not including bottom data
data = df[0:64]


# In[14]:


#Place the data values into a list
data = data.values.tolist()


# In[15]:


# create food dictionary
foods = [x[0] for x in data]
calories = dict([(x[0], float(x[3])) for x in data])
cholesterol = dict([(x[0], float(x[4])) for x in data])
totalFat = dict([(x[0], float(x[5])) for x in data])
sodium = dict([(x[0], float(x[6])) for x in data])
carbs = dict([(x[0], float(x[7])) for x in data])
fiber = dict([(x[0], float(x[8])) for x in data])
protien = dict([(x[0], float(x[9])) for x in data])
vitaminA = dict([(x[0], float(x[10])) for x in data])
vitaminC = dict([(x[0], float(x[11])) for x in data])
calcium = dict([(x[0], float(x[12])) for x in data])
iron = dict([(x[0], float(x[13])) for x in data])


# In[16]:


# Create lists for Mininum and Maximum daily intake  (from 67 and 68 rows of the given data )
minimum = [1500, 30, 20, 800, 130, 125, 60, 1000, 400, 700, 10]
maximum=[2500, 240, 70, 2000, 450, 250, 100, 10000, 5000, 1500, 40]


# In[17]:


# Append the mininum and maximum constraints to each column of the food dictionary
M = []
for j in range(0,11):
    M.append(dict([(x[0], float(x[j+3])) for x in data]))


# In[18]:


#define the cost - food dictionary
cost = dict([(x[0], float(x[1])) for x in data])


# In[19]:


#Giving a name to the optimization problem and telling it that the goal is to minimize the cost
OptimizationProblem = LpProblem('PuLPTutorial', LpMinimize)


# In[20]:


# Defining Variables
# xi = amount of food in the diet and x cannot go below 0. 
x=LpVariable.dicts("Amounts", foods, 0) 


# In[21]:


#yi = Binary variable indicating if the food i is used (0 or 1 are the possibilities)
y=LpVariable.dicts("Chosen",foods,0,1,"Binary")


# In[22]:


#Defining constraints

#a.If a food is selected, then a minimum of 1/10 serving must be chosen. 
# the food also need to be less than a large amount. I have arbitarily chooses 100000
for i in foods:
    OptimizationProblem += x[i] <= 100000 * y[i]
    OptimizationProblem += x[i]  >= 0.1 * y[i]


# In[23]:


#b.	Many people dislike celery and frozen broccoli. So at most one, but not both, can be selected.
OptimizationProblem +=y['Frozen Broccoli'] + y['Celery, Raw'] <= 1


# In[24]:


#c.	To get day-to-day variety in protein, at least 3 kinds of meat/poultry/fish/eggs must
# be selected. 

OptimizationProblem += y['Roasted Chicken'] + y['Poached Eggs'] +  y['Frankfurter, Beef'] +   y['Scrambled Eggs'] + y['Hamburger W/Toppings'] + y['Pizza W/Pepperoni']+ y['Bologna,Turkey']  + y['Kielbasa,Prk'] +y['White Tuna in Water'] +y['Sardines in Oil'] + y['Ham,Sliced,Extralean'] +  y['Hotdog, Plain'] + y['Pork']  +y['Chicknoodl Soup']+y['Splt Pea&Hamsoup'] + y['Vegetbeef Soup'] + y['Beanbacn Soup,W/Watr']>= 3


# In[25]:


# Add  Mininium / Maximum calory intake Constraints to all the foods
for i in range(0,11):
    dotproductM = pulp.lpSum([M[i][j] * x[j] for j in foods])
    minumum_Condition = minimum[i] <= + dotproductM
    OptimizationProblem += minumum_Condition
    
for i in range(0,11):
    dotproductM = pulp.lpSum([M[i][j] * x[j] for j in foods])
    maximum_Condition = maximum[i] >= + dotproductM
    OptimizationProblem += maximum_Condition


# In[26]:


# Objective Function : minimize the cost of the food 
# define the objective function
OptimizationProblem += lpSum([cost[i] * x[i] for i in foods])


# In[27]:


# Solve the Optimization Problem
OptimizationProblem.solve()


# In[28]:


#Optimal Solution
print('Optimization Solution is:')
for item in OptimizationProblem.variables():
    if item.varValue > 0 :
        if str(item).find('Chosen'):
            print(str(item.varValue) + "units of " + str(item))
#Cost of the Optimal Diet is 
print("Total cost of food = $%.2f" %value(OptimizationProblem.objective))


# In[ ]:




