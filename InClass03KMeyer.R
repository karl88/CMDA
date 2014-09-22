#09.22.2014
#Karl Meyer
#CMDA 3654
#In Class 03
#09.22.2014


getwd()
setwd("C:\\Users\\Karl Meyer\\Downloads")
load("exampleData1.rData")

#Problem 1: 

#Merge the median income dataset and custdata dataset
#create a new variable called "norm.income" by scaling the "income" variable. 
#Use the appropriate median income for scaling
merged <- merge(custdata, medianincome)
median.income <- mean(merged$income)
merged$norm.income <- merged$income/median.income

#Get Numerical Summaries of the norm.income variable
summary(merged$norm.income)
hist(merged$income, breaks = 7)
hist(merged$norm.income, breaks = 7)
#This normalization would make sense when someone needs to compare whether a new job or
#a relocation would result in them making more or less money relative to their living expenses


#Problem 2: Split the merged dataset into 30% training and 70% testing sets

merged$gp <- runif(dim(merged)[1])
trainingSet <- subset(merged, merged$gp <= 0.3)
testSet <- subset(merged, merged$gp > 0.3)
