# Filename: house_prices_project.R
# Author: CSC 3220 House Prices Team
# Date: 11/8/2021
# Purpose: 

# Load prerequisite packages
library(readr)
library(ggplot2)

# Load in data
test = read_csv("test.csv")
train = read_csv("train.csv")

y = train$SalePrice

#--------------------------------------------------
#                       EDA
#--------------------------------------------------
summary(train)

# Note: test and train hists similar, good sign

# Histograms for numerical data
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

# Plot numeric variables
par(mfrow = c(2,2))
plot(y ~ train$Id)                # Appears to follow a flat horizontal line across the graph, indicating no relationship to y
plot(y ~ train$MSSubClass)        # Since this is really a classification, not a numeric, it makes sense for there to be distinct vertical lines formed by observations at only specific x's
plot(y ~ train$LotFrontage)       # Outliers past 300 on the x axis, mostly scattered but slight upward trend below 200
plot(y ~ train$LotArea)           # Outliers past 100000, data 

plot(y ~ train$OverallQual)
plot(y ~ train$OverallCond)
plot(y ~ train$YearBuilt)
plot(y ~ train$YearRemodAdd)

plot(y ~ train$MasVnrArea)
plot(y ~ train$BsmtFinSF1)
plot(y ~ train$BsmtFinSF2)
plot(y ~ train$BsmtUnfSF)

plot(y ~ train$TotalBsmtSF)
plot(y ~ train$`1stFlrSF`)
plot(y ~ train$`2ndFlrSF`)
plot(y ~ train$LowQualFinSF)

plot(y ~ train$GrLivArea)
plot(y ~ train$BsmtFullBath)
plot(y ~ train$BsmtHalfBath)
plot(y ~ train$FullBath)

plot(y ~ train$HalfBath)
plot(y ~ train$BedroomAbvGr)
plot(y ~ train$KitchenAbvGr)
plot(y ~ train$TotRmsAbvGrd)

plot(y ~ train$Fireplaces)
plot(y ~ train$GarageYrBlt)
plot(y ~ train$GarageCars)
plot(y ~ train$GarageArea)

plot(y ~ train$WoodDeckSF)
plot(y ~ train$OpenPorchSF)
plot(y ~ train$EnclosedPorch)
plot(y ~ train$`3SsnPorch`)

plot(y ~ train$ScreenPorch)
plot(y ~ train$PoolArea)
plot(y ~ train$MiscVal)

par(mfrow = c(2,1))
plot(y ~ train$MoSold)
plot(y ~ train$YrSold)

par(mfrow = c(1,1))
