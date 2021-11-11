# Filename: house_prices_project.R
# Author: CSC 3220 House Prices Team
# Date: 11/8/2021
# Purpose: 

# Load prerequisite packages
library(readr)
library(ggplot2)

# Load in data
test <- read_csv("C:\\Users\\amina\\Downloads\\New folder (5)\\test.csv")
train = read_csv("C:\\Users\\amina\\Downloads\\New folder (5)\\train.csv")

#-------------------
summary(test)
hist(train$Id) #id got a wierd distribution
hist((train$MSSubClass)) # the shape is mostly falling down to the rightware
hist(train$LotFrontage)
hist(train$LotArea)
hist(train$OverallCond)
hist(train$OverallQual)
hist(train$YearBuilt)
hist(train$YearRemodAdd)
hist(train$MasVnrArea)
hist(train$BsmtFinSF1)
hist(train$BsmtFinSF2)
hist(train$BsmtUnfSF)
hist(train$TotalBsmtSF)
hist(train$LowQualFinSF)
hist(train$GrLivArea)


