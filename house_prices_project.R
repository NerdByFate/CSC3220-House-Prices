# Filename: house_prices_project.R
# Author: CSC 3220 House Prices Team
# Date: 11/15/2021
# Purpose: 

# -------------------------------------------------
#                   Prerequisites
# -------------------------------------------------

# Load libraries
library(readr)
library(ggplot2)

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

# Replace NAs (Mean Imputation)
train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage, na.rm = TRUE)
train$MasVnrArea[is.na(train$MasVnrArea)] = mean(train$MasVnrArea, na.rm = TRUE)
train$GarageYrBlt[is.na(train$GarageYrBlt)] = mean(train$GarageYrBlt, na.rm = TRUE)

test$LotFrontage[is.na(test$LotFrontage)] = mean(train$LotFrontage, na.rm = TRUE)
test$MasVnrArea[is.na(test$MasVnrArea)] = mean(train$MasVnrArea, na.rm = TRUE)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = mean(train$BsmtFinSF1, na.rm = TRUE)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = mean(train$BsmtUnfSF, na.rm = TRUE)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = mean(train$BsmtUnfSF, na.rm = TRUE)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = mean(train$TotalBsmtSF, na.rm = TRUE)
test$BsmtFullBath[is.na(test$BsmtFullBath)] = mean(train$BsmtFullBath, na.rm = TRUE)  # Mean imputation in the test set should be done on the train means to avoid data leakage
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = mean(train$BsmtHalfBath, na.rm = TRUE)
test$GarageYrBlt[is.na(test$GarageYrBlt)] = mean(train$GarageYrBlt, na.rm = TRUE)
test$GarageArea[is.na(test$GarageArea)] = mean(train$GarageArea, na.rm = TRUE)
test$GarageCars[is.na(test$GarageCars)] = mean(train$GarageCars, na.rm = TRUE)

# Replace NAs (with classifiers)
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
train$MasVnrType[is.na(train$MasVnrType)] = "Unknown" # From here on, the meaning of NA is not clarified on Kaggle, indicating that these are actually unkown values
train$Electrical[is.na(train$Electrical)] = "Unknown"

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
test$MasVnrType[is.na(test$MasVnrType)] = "Unknown"
test$MSZoning[is.na(test$MSZoning)] = "Unknown"
test$Utilities[is.na(test$Utilities)] = "Unknown"
test$Exterior1st[is.na(test$Exterior1st)] = "Unknown"
test$Exterior2nd[is.na(test$Exterior2nd)] = "Unknown"
test$KitchenQual[is.na(test$KitchenQual)] = "Unknown"
test$Functional[is.na(test$Functional)] = "Unknown"
test$SaleType[is.na(test$SaleType)] = "Unknown"

# Check for remaining NAs in train
for(i in 1:length(names(train))) {
  print(c(i, sum(is.na(train[,i]))))
}

# Check for remaining NAs in test
for(i in 1:length(names(test))) {
  print(c(i, sum(is.na(test[,i]))))
}

# TODO: Likely need to convert characters to factors for use in linear regression

# -------------------------------------------------
#                       EDA
# -------------------------------------------------
summary(train)

# Note: test and train hists similar, good sign

# Histograms/boxplots for numerical data
par(mfrow = c(1,2))

hist(train$Id)                    # id got a wierd distribution
boxplot(train$Id)

hist((train$MSSubClass))          # the shape is mostly falling down to the rightware
boxplot(train$MSSubClass)

hist(train$LotFrontage)
boxplot(train$LotFrontage)

hist(train$LotArea)
boxplot(train$LotArea)

hist(train$OverallQual)
boxplot(tran$OverallQual)

hist(train$OverallCond)
boxplot(train$OverallCond)

hist(train$YearBuilt)
boxplot(train$YearBuilt)

hist(train$YearRemodAdd)
boxplot(train$YearRemodAdd)

hist(train$MasVnrArea)
boxplot(train$MasVnrArea)

hist(train$BsmtFinSF1)
boxplot(train$BsmtFinSF1)

hist(train$BsmtFinSF2)
boxplot(train$BsmtFinSF2)

hist(train$BsmtUnfSF)
boxplot(train$BsmtUnfSF)

hist(train$TotalBsmtSF)
boxplot(train$TotalBsmtSF)

hist(train$`1stFlrSF`)
boxplot(train$`1stFlrSF`)

hist(train$`2ndFlrSF`)
boxplot(train$`2ndFlrSF`)

hist(train$LowQualFinSF)
boxplot(train$LowQualFinSF)

hist(train$GrLivArea)
boxplot(train$GRLivArea)

hist(train$BsmtFullBath)
boxplot(train$BsmtFullBath)

hist(train$BsmtHalfBath)
boxplot(train$BsmtHalfBath)

hist(train$FullBath)
boxplot(train$FullBath)

hist(train$HalfBath)
boxplot(train$HalfBath)

hist(train$BedroomAbvGr)
boxplot(train$BedroomAbvGr)

hist(train$KitchenAbvGr)
boxplot(train$KitchenAbvGr)

hist(train$TotRmsAbvGrd)
boxplot(train$TotRmsAbvGrd)

hist(train$Fireplaces)
boxplot(train$Fireplaces)

hist(train$GarageYrBlt)
boxplot(train$GarageYrBlt)

hist(train$GarageCars)
boxplot(train$GarageCars)

hist(train$GarageArea)
boxplot(train$GarageArea)

hist(train$WoodDeckSF)
boxplot(train$WoodDeckSF)

hist(train$OpenPorchSF)
boxplot(train$OpenPorchSF)

hist(train$EnclosedPorch)
boxplot(train$EnclosedPorch)

hist(train$`3SsnPorch`)
boxplot(train$`3SsnPorch`)

hist(train$ScreenPorch)
boxplot(train$ScreenPorch)

hist(train$PoolArea)
boxplot(train$PoolArea)

hist(train$MiscVal)
boxplot(train$MiscVal)

hist(train$MoSold)
boxplot(train$MoSold)

hist(train$YrSold)
boxplot(train$YrSold)

hist(train$SalePrice)
boxplot(train$SalePrice)

par(mfrow = c(1,1))

# Bar charts for categorical data
barplot(train$MSZoning)
barplot(train$Street)
barplot(train$Alley)
barplot(train$LotShape)
barplot(train$LandContour)
barplot(train$Utilities)
barplot(train$LotConfig)
barplot(train$LandSlope)
barplot(train$Neighborhood)
barplot(train$Condition1)
barplot(train$Condition2)
barplot(train$BldgType)
barplot(train$HouseStyle)
barplot(train$RoofStyle)
barplot(train$RoofMatl)
barplot(train$Exterior1st)
barplot(train$Exterior2nd)
barplot(train$MasVnrType)
barplot(train$ExterQual)
barplot(train$ExterCond)
barplot(train$Foundation)
barplot(train$BsmtQual)
barplot(train$BsmtCond)
barplot(train$BsmtExposure)
barplot(train$BsmtFinType1)
barplot(train$BsmtFinType2)
barplot(train$Heating)
barplot(train$HeatingQC)
barplot(train$CentralAir)
barplot(train$Electrical)
barplot(train$KitchenQual)
barplot(train$Functional)
barplot(train$FireplaceQu)
barplot(train$GarageType)
barplot(train$GarageFinish)
barplot(train$GarageQual)
barplot(train$GarageCond)
barplot(train$PavedDrive)
barplot(train$PoolQC)
barplot(train$Fence)
barplot(train$MiscFeature)
barplot(train$SaleType)

# Plot numeric variables against y
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

par(mfrow = c(1,2))
plot(y ~ train$MoSold)
plot(y ~ train$YrSold)

par(mfrow = c(1,1))

# -------------------------------------------------
#                   Data Cleaning
# -------------------------------------------------

