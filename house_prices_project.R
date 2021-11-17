# Filename: house_prices_project.R
# Author: CSC 3220 House Prices Team
# Date: 11/16/2021
# Purpose: A script to create and evaluate models that predict the final price of homes based on given variables

# -------------------------------------------------
#                   Prerequisites
# -------------------------------------------------

# Install libraries
install.packages("readr")
install.packages("dplyr")
install.packages("VIM")

# Load libraries
library(readr)
library(dplyr)
library(VIM)

# Load data
test = read_csv("test.csv")
train = read_csv("train.csv")

y = train$SalePrice

# -------------------------------------------------
#                   Data Cleaning
# -------------------------------------------------

# Identify NAs
summary(train)
summary(test)

# Replace NAs (with classifiers identified on Kaggle)
train$Alley[is.na(train$Alley)] = "NoAccess"
train$BsmtQual[is.na(train$BsmtQual)] = "NB"          # No Basement
train$BsmtCond[is.na(train$BsmtCond)] = "NB"
train$BsmtExposure[is.na(train$BsmtExposure)] = "NB"
train$BsmtFinType1[is.na(train$BsmtFinType1)] = "NB"
train$BsmtFinType2[is.na(train$BsmtFinType2)] = "NB"
train$FireplaceQu[is.na(train$FireplaceQu)] = "NF"    # No Fireplace
train$GarageType[is.na(train$GarageType)] = "NG"      # No Garage
train$GarageFinish[is.na(train$GarageFinish)] = "NG"
train$GarageQual[is.na(train$GarageQual)] = "NG"
train$GarageCond[is.na(train$GarageCond)] = "NG"
train$PoolQC[is.na(train$PoolQC)] = "NP" # No Pool
train$Fence[is.na(train$Fence)] = "NoFence"
train$MiscFeature[is.na(train$MiscFeature)] = "None"

test$Alley[is.na(test$Alley)] = "NoAccess"
test$BsmtQual[is.na(test$BsmtQual)] = "NB"
test$BsmtCond[is.na(test$BsmtCond)] = "NB"
test$BsmtExposure[is.na(test$BsmtExposure)] = "NB"
test$BsmtFinType1[is.na(test$BsmtFinType1)] = "NB"
test$BsmtFinType2[is.na(test$BsmtFinType2)] = "NB"
test$FireplaceQu[is.na(test$FireplaceQu)] = "NF"
test$GarageType[is.na(test$GarageType)] = "NG"
test$GarageFinish[is.na(test$GarageFinish)] = "NG"
test$GarageQual[is.na(test$GarageQual)] = "NG"
test$GarageCond[is.na(test$GarageCond)] = "NG"
test$PoolQC[is.na(test$PoolQC)] = "NP"
test$Fence[is.na(test$Fence)] = "NoFence"
test$MiscFeature[is.na(test$MiscFeature)] = "None"

# Replace NAs (kNN Imputation)
k = round(sqrt(length(train)), 0)
train = kNN(data = train, dist_var = colnames(x = train %>% select(!SalePrice)), k = k, imp_var = FALSE)
temp = train # Logic from here is only necessary because we want to perform kNN for the test set based on the train set as to not cause data leakage
testNAs = which(is.na(test), arr.ind=TRUE)
for(i in 1:(length(testNAs)/2)) {
  temp[testNAs[i, 1], testNAs[i, 2]] = NA
}
temp = kNN(data = temp, dist_var = colnames(temp %>% select(!SalePrice)), k = k, imp_var = FALSE)
for(i in 1:(length(testNAs)/2)) {
  test[testNAs[i, 1], testNAs[i, 2]] = temp[testNAs[i, 1], testNAs[i, 2]]
}

# Check for remaining NAs in train
for(i in 1:length(names(train))) {
  print(c(i, sum(is.na(train[,i]))))
}

# Check for remaining NAs in test
for(i in 1:length(names(test))) {
  print(c(i, sum(is.na(test[,i]))))
}

# We don't need to convert categorical data to factors as this will be done implicitly by lm()

# -------------------------------------------------
#                       EDA
# -------------------------------------------------
summary(train)

# Note: test and train hists similar; good sign

# Histograms/boxplots for numerical data
par(mfrow = c(1,2))

hist(train$Id, main = "Histogram")              # distributed evenly across the tuples, which makes sense as IDs should be unique
boxplot(train$Id, main = "Boxplot")

hist((train$MSSubClass), main = "Histogram")    # right skewed with outliers past 150; however, these should not be removed as this variable is effectively categorical
boxplot(train$MSSubClass, main = "Boxplot")

hist(train$LotFrontage, main = "Histogram")
boxplot(train$LotFrontage, main = "Boxplot")

hist(train$LotArea, main = "Histogram")
boxplot(train$LotArea, main = "Boxplot")

hist(train$OverallQual, main = "Histogram")
boxplot(train$OverallQual, main = "Boxplot")

hist(train$OverallCond, main = "Histogram")
boxplot(train$OverallCond, main = "Boxplot")

hist(train$YearBuilt, main = "Histogram")
boxplot(train$YearBuilt, main = "Boxplot")

hist(train$YearRemodAdd, main = "Histogram")
boxplot(train$YearRemodAdd, main = "Boxplot")

hist(train$MasVnrArea, main = "Histogram")
boxplot(train$MasVnrArea, main = "Boxplot")

hist(train$BsmtFinSF1, main = "Histogram")
boxplot(train$BsmtFinSF1, main = "Boxplot")

hist(train$BsmtFinSF2, main = "Histogram")
boxplot(train$BsmtFinSF2, main = "Boxplot")

hist(train$BsmtUnfSF, main = "Histogram")
boxplot(train$BsmtUnfSF, main = "Boxplot")

hist(train$TotalBsmtSF, main = "Histogram")
boxplot(train$TotalBsmtSF, main = "Boxplot")

hist(train$`1stFlrSF`, main = "Histogram")
boxplot(train$`1stFlrSF`, main = "Boxplot")

hist(train$`2ndFlrSF`, main = "Histogram")
boxplot(train$`2ndFlrSF`, main = "Boxplot")

hist(train$LowQualFinSF, main = "Histogram")
boxplot(train$LowQualFinSF, main = "Boxplot")

hist(train$GrLivArea, main = "Histogram")
boxplot(train$GrLivArea, main = "Boxplot")

hist(train$BsmtFullBath, main = "Histogram")
boxplot(train$BsmtFullBath, main = "Boxplot")

hist(train$BsmtHalfBath, main = "Histogram")
boxplot(train$BsmtHalfBath, main = "Boxplot")

hist(train$FullBath, main = "Histogram")
boxplot(train$FullBath, main = "Boxplot")

hist(train$HalfBath, main = "Histogram")
boxplot(train$HalfBath, main = "Boxplot")

hist(train$BedroomAbvGr, main = "Histogram")
boxplot(train$BedroomAbvGr, main = "Boxplot")

hist(train$KitchenAbvGr, main = "Histogram")
boxplot(train$KitchenAbvGr, main = "Boxplot")

hist(train$TotRmsAbvGrd, main = "Histogram")
boxplot(train$TotRmsAbvGrd, main = "Boxplot")

hist(train$Fireplaces, main = "Histogram")
boxplot(train$Fireplaces, main = "Boxplot")

hist(train$GarageYrBlt, main = "Histogram")
boxplot(train$GarageYrBlt, main = "Boxplot")

hist(train$GarageCars, main = "Histogram")
boxplot(train$GarageCars, main = "Boxplot")

hist(train$GarageArea, main = "Histogram")
boxplot(train$GarageArea, main = "Boxplot")

hist(train$WoodDeckSF, main = "Histogram")
boxplot(train$WoodDeckSF, main = "Boxplot")

hist(train$OpenPorchSF, main = "Histogram")
boxplot(train$OpenPorchSF, main = "Boxplot")

hist(train$EnclosedPorch, main = "Histogram")
boxplot(train$EnclosedPorch, main = "Boxplot")

hist(train$`3SsnPorch`, main = "Histogram")
boxplot(train$`3SsnPorch`, main = "Boxplot")

hist(train$ScreenPorch, main = "Histogram")
boxplot(train$ScreenPorch, main = "Boxplot")

hist(train$PoolArea, main = "Histogram")
boxplot(train$PoolArea, main = "Boxplot")

hist(train$MiscVal, main = "Histogram")
boxplot(train$MiscVal, main = "Boxplot")

hist(train$MoSold, main = "Histogram")
boxplot(train$MoSold, main = "Boxplot")

hist(train$YrSold, main = "Histogram")
boxplot(train$YrSold, main = "Boxplot")

hist(train$SalePrice, main = "Histogram")
boxplot(train$SalePrice, main = "Boxplot")

par(mfrow = c(1,1))

# Bar charts for categorical data
barplot(table(train$MSZoning))
barplot(table(train$Street))
barplot(table(train$Alley))
barplot(table(train$LotShape))
barplot(table(train$LandContour))
barplot(table(train$Utilities))
barplot(table(train$LotConfig))
barplot(table(train$LandSlope))
barplot(table(train$Neighborhood))
barplot(table(train$Condition1))
barplot(table(train$Condition2))
barplot(table(train$BldgType))
barplot(table(train$HouseStyle))
barplot(table(train$RoofStyle))
barplot(table(train$RoofMatl))
barplot(table(train$Exterior1st))
barplot(table(train$Exterior2nd))
barplot(table(train$MasVnrType))
barplot(table(train$ExterQual))
barplot(table(train$ExterCond))
barplot(table(train$Foundation))
barplot(table(train$BsmtQual))
barplot(table(train$BsmtCond))
barplot(table(train$BsmtExposure))
barplot(table(train$BsmtFinType1))
barplot(table(train$BsmtFinType2))
barplot(table(train$Heating))
barplot(table(train$HeatingQC))
barplot(table(train$CentralAir))
barplot(table(train$Electrical))
barplot(table(train$KitchenQual))
barplot(table(train$Functional))
barplot(table(train$FireplaceQu))
barplot(table(train$GarageType))
barplot(table(train$GarageFinish))
barplot(table(train$GarageQual))
barplot(table(train$GarageCond))
barplot(table(train$PavedDrive))
barplot(table(train$PoolQC))
barplot(table(train$Fence))
barplot(table(train$MiscFeature))
barplot(table(train$SaleType))

# -------------------------------------------------
#                More Data Cleaning
# -------------------------------------------------

# Outlier removal (where necessary)
