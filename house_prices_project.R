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
install.packages("corrplot")
install.packages("RColorBrewer")

# Load libraries
library(readr)
library(dplyr)
library(VIM)
library(corrplot)
library(RColorBrewer)

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
temp = train %>% select(!SalePrice) # Logic from here is only necessary because we want to perform kNN for the test set based on the train set as to not cause data leakage
temp2 = test[rowSums(is.na(test)) > 0, ]
temp = rbind(temp, temp2)
testNAs = which(is.na(test), arr.ind=TRUE)
tempNAs = which(is.na(temp), arr.ind=TRUE)
temp = kNN(data = temp, dist_var = colnames(temp), k = k, imp_var = FALSE)
for(i in 1:(length(testNAs)/2)) {
  test[testNAs[i, 1], testNAs[i, 2]] = temp[tempNAs[i, 1], tempNAs[i, 2]]
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

hist(train$Id, main = "Histogram", xlab = "ID")                       # Distributed evenly across the tuples, which makes sense as IDs should be unique
boxplot(train$Id, main = "Boxplot", ylab = "ID")

hist((train$MSSubClass), main = "Histogram", xlab = "MSSubClass")     # Right skewed with outliers past 150; no obvious erronous data
boxplot(train$MSSubClass, main = "Boxplot", ylab = "MSSubClass")

hist(train$LotFrontage, main = "Histogram", xlab = "LotFrontage")     # Outliers past 300, but these lots are also incredibly large upon closer inspection, so this seems plausible
boxplot(train$LotFrontage, main = "Boxplot", ylab = "LotFrontage")

hist(train$LotArea, main = "Histogram", xlab = "LotArea")             # Most lots are very small, so there are many outliers but these are within the realm of plausibility
boxplot(train$LotArea, main = "Boxplot", ylab = "LotArea")

hist(train$OverallQual, main = "Histogram", xlab = "OverallQual")     # Relatively normal distribution, no notable outliers
boxplot(train$OverallQual, main = "Boxplot", ylab = "OverallQual")

hist(train$OverallCond, main = "Histogram", xlab = "OverallCond")     # Categorical, slightly right skewed, nothing outside of acceptable range
boxplot(train$OverallCond, main = "Boxplot", ylab = "OverallCond")

hist(train$YearBuilt, main = "Histogram", xlab = "YearBuilt")         # Most houses in Ames seem to have been built recently, with especially old houses actually being an outlier; no values seem erroneous though
boxplot(train$YearBuilt, main = "Boxplot", ylab = "YearBuilt")

hist(train$YearRemodAdd, main = "Histogram", xlab = "YearRemodAdd")   # Two peaks at either end of the spectrum, though no outlying data
boxplot(train$YearRemodAdd, main = "Boxplot", ylab = "YearRemodAdd")

hist(train$MasVnrArea, main = "Histogram", xlab = "MasVnrArea")       # Most houses do not have masonry veneers, so it makes sense that this histogram is centered on this value; some erronous data was found based on this
boxplot(train$MasVnrArea, main = "Boxplot", ylab = "MasVnrArea")

hist(train$BsmtFinSF1, main = "Histogram", xlab = "BsmtFinSF1")       # Right skewed, some outliers past 5000 square feet, but this is still plausible
boxplot(train$BsmtFinSF1, main = "Boxplot", ylab = "BsmtFinSF1")

hist(train$BsmtFinSF2, main = "Histogram", xlab = "BsmtFinSF2")       # Similarly right skewed, though even more focused around the lower values; erroneous values in relation to basement type
boxplot(train$BsmtFinSF2, main = "Boxplot", ylab = "BsmtFinSF2")

hist(train$BsmtUnfSF, main = "Histogram", xlab = "BsmtUnfSF")         # Right skewed, no obvious erroneous data
boxplot(train$BsmtUnfSF, main = "Boxplot", ylab = "BsmtUnfSF")

hist(train$TotalBsmtSF, main = "Histogram", xlab = "TotalBsmtSF")     # Outlier past 5000 square feet, though this is plausible
boxplot(train$TotalBsmtSF, main = "Boxplot", ylab = "TotalBsmtSF")

hist(train$`1stFlrSF`, main = "Histogram", xlab = "1stFlrSF")         # Relatively normally distributed, outliers past 4000 square feet are plausible
boxplot(train$`1stFlrSF`, main = "Boxplot", ylab = "1stFlrSF")

hist(train$`2ndFlrSF`, main = "Histogram", xlab = "2ndFlrSF")         # It would appear that most houses do not have a second floor; moreover, there are erroneous entries that need correcting
boxplot(train$`2ndFlrSF`, main = "Boxplot", ylab = "2ndFlrSF")

hist(train$LowQualFinSF, main = "Histogram", xlab = "LowQualFinSF")   # Very few houses with low quality square footage sold, which would make sense; those that did likely had other attractive features
boxplot(train$LowQualFinSF, main = "Boxplot", ylab = "LowQualFinSF")

hist(train$GrLivArea, main = "Histogram", xlab = "GrLivArea")         # Relatively normally distributed, no obvious erroneous data
boxplot(train$GrLivArea, main = "Boxplot", ylab = "GrLivArea")

hist(train$BsmtFullBath, main = "Histogram", xlab = "BsmtFullBath")   # All relatively normal amounts
boxplot(train$BsmtFullBath, main = "Boxplot", ylab = "BsmtFullBath")

hist(train$BsmtHalfBath, main = "Histogram", xlab = "BsmtHalfBath")   # Again, all relatively normal amounts
boxplot(train$BsmtHalfBath, main = "Boxplot", ylab = "BsmtHalfBath")

hist(train$FullBath, main = "Histogram", xlab = "FullBath")           # Relatively normal amounts
boxplot(train$FullBath, main = "Boxplot", ylab = "FullBath")

hist(train$HalfBath, main = "Histogram", xlab = "HalfBath")           # Relatively normal amounts
boxplot(train$HalfBath, main = "Boxplot", ylab = "HalfBath")

hist(train$BedroomAbvGr, main = "Histogram", xlab = "BedroomAbvGr")   # Several houses with no bedrooms above ground, though these do have basements so it is plausible
boxplot(train$BedroomAbvGr, main = "Boxplot", ylab = "BedroomAbvGr")

hist(train$KitchenAbvGr, main = "Histogram", xlab = "KitchenAbvGr")   # One house with no kitchen above ground, though it does have a basement
boxplot(train$KitchenAbvGr, main = "Boxplot", ylab = "KitchenAbvGr")

hist(train$TotRmsAbvGrd, main = "Histogram", xlab = "TotRmsAbvGrd")   # Relatively normal distribution, no obvious erroneous data
boxplot(train$TotRmsAbvGrd, main = "Boxplot", ylab = "TotRmsAbvGrd")

hist(train$Fireplaces, main = "Histogram", xlab = "Fireplaces")       # Most houses have either no or one fireplace(s), which seems logical; no obvious erroneous data
boxplot(train$Fireplaces, main = "Boxplot", ylab = "Fireplaces")

hist(train$GarageYrBlt, main = "Histogram", xlab = "GarageYrBlt")     # Most garages built recently, which makes sense as most houses are also built recently
boxplot(train$GarageYrBlt, main = "Boxplot", ylab = "GarageYrBlt")

hist(train$GarageCars, main = "Histogram", xlab = "GarageCars")       # Relatively normal values
boxplot(train$GarageCars, main = "Boxplot", ylab = "GarageCars")

hist(train$GarageArea, main = "Histogram", xlab = "GarageArea")       # Relatively normal distribution, no obvious erroneous data
boxplot(train$GarageArea, main = "Boxplot", ylab = "GarageArea")

hist(train$WoodDeckSF, main = "Histogram", xlab = "WoodDeckSF")       # Heavily right-skewed, which would seem to indicate most houses have either no or small decks
boxplot(train$WoodDeckSF, main = "Boxplot", ylab = "WoodDeckSF")

hist(train$OpenPorchSF, main = "Histogram", xlab = "OpenPorchSF")     # Much the same as the situaton with deck size
boxplot(train$OpenPorchSF, main = "Boxplot", ylab = "OpenPorchSF")

hist(train$EnclosedPorch, main = "Histogram", xlab = "EnclosedPorch") # Similar to deck and porch size, except it would seem even less homes have enclosed porches
boxplot(train$EnclosedPorch, main = "Boxplot", ylab = "EnclosedPorch")

hist(train$`3SsnPorch`, main = "Histogram", xlab = "3SsnPorch")       # Even less homes have three season porch areas than enclosed porches
boxplot(train$`3SsnPorch`, main = "Boxplot", ylab = "3SsnPorch")

hist(train$ScreenPorch, main = "Histogram", xlab = "ScreenPorch")     # Much the same as enclosed porch size
boxplot(train$ScreenPorch, main = "Boxplot", ylab = "ScreenPorch")

hist(train$PoolArea, main = "Histogram", xlab = "PoolArea")           # Most houses have either no or small pools, yielding a histogram where only the bar near 0 is even visible; still, no obvious erroneous data
boxplot(train$PoolArea, main = "Boxplot", ylab = "PoolArea")

hist(train$MiscVal, main = "Histogram", xlab = "MiscVal")             # Most houses don't have miscellaneous value
boxplot(train$MiscVal, main = "Boxplot", ylab = "MiscVal")

hist(train$MoSold, main = "Histogram", xlab = "MoSold")               # Relatively normal distribution, no outliers
boxplot(train$MoSold, main = "Boxplot", ylab = "MoSold")

hist(train$YrSold, main = "Histogram", xlab = "YrSold")               # Fairly evenly distributed (even around the housing crisis, interestingly enough), no outliers
boxplot(train$YrSold, main = "Boxplot", ylab = "YrSold")

hist(train$SalePrice, main = "Histogram", xlab = "SalePrice")         # Somewhat right skewed, dependent variable
boxplot(train$SalePrice, main = "Boxplot", ylab = "SalePrice")

par(mfrow = c(1,1))

# Bar charts for categorical data
barplot(table(train$MSZoning), main = "MSZoning Bar Chart", xlab = "MSZoning", ylab = "Frequency")
barplot(table(train$Street), main = "Street Bar Chart", xlab = "Street", ylab = "Frequency")
barplot(table(train$Alley), main = "Alley Bar Chart", xlab = "Alley", ylab = "Frequency")
barplot(table(train$LotShape), main = "LotShape Bar Chart", xlab = "LotShape", ylab = "Frequency")
barplot(table(train$LandContour), main = "LandContour Bar Chart", xlab = "LandContour", ylab = "Frequency")
barplot(table(train$Utilities), main = "Utilities Bar Chart", xlab = "Utilities", ylab = "Frequency")
barplot(table(train$LotConfig), main = "LotConfig Bar Chart", xlab = "LotConfig", ylab = "Frequency")
barplot(table(train$LandSlope), main = "LandSlope Bar Chart", xlab = "LandSlope", ylab = "Frequency")
barplot(table(train$Neighborhood), main = "Neighborhood Bar Chart", xlab = "Neighborhood", ylab = "Frequency")
barplot(table(train$Condition1), main = "Condition1 Bar Chart", xlab = "Condition1", ylab = "Frequency")
barplot(table(train$Condition2), main = "Condition2 Bar Chart", xlab = "Condition2", ylab = "Frequency")
barplot(table(train$BldgType), main = "BldgType Bar Chart", xlab = "BldgType", ylab = "Frequency")
barplot(table(train$HouseStyle), main = "HouseStyle Bar Chart", xlab = "HouseStyle", ylab = "Frequency")
barplot(table(train$RoofStyle), main = "RoofStyle Bar Chart", xlab = "RoofStyle", ylab = "Frequency")
barplot(table(train$RoofMatl), main = "RoofMatl Bar Chart", xlab = "RoofMatl", ylab = "Frequency")
barplot(table(train$Exterior1st), main = "Extorior1st Bar Chart", xlab = "Exterior1st", ylab = "Frequency")
barplot(table(train$Exterior2nd), main = "Exterior2nd Bar Chart", xlab = "Exterior2nd", ylab = "Frequency")
barplot(table(train$MasVnrType), main = "MasVnrType Bar Chart", xlab = "MasVnrType", ylab = "Frequency")
barplot(table(train$ExterQual), main = "ExterQual Bar Chart", xlab = "ExterQual", ylab = "Frequency")
barplot(table(train$ExterCond), main = "ExterCond Bar Chart", xlab = "ExterCond", ylab = "Frequency")
barplot(table(train$Foundation), main = "Foundation Bar Chart", xlab = "Foundation", ylab = "Frequency")
barplot(table(train$BsmtQual), main = "BsmtQual Bar Chart", xlab = "BsmtQual", ylab = "Frequency")
barplot(table(train$BsmtCond), main = "BsmtCond Bar Chart", xlab = "BsmtCond", ylab = "Frequency")
barplot(table(train$BsmtExposure), main = "BsmtExposure Bar Chart", xlab = "BsmtExposure", ylab = "Frequency")
barplot(table(train$BsmtFinType1), main = "BsmtFinType1 Bar Chart", xlab = "BsmtFinType1", ylab = "Frequency")
barplot(table(train$BsmtFinType2), main = "BsmtFinType2 Bar Chart", xlab = "BsmtFinType2", ylab = "Frequency")
barplot(table(train$Heating), main = "Heating Bar Chart", xlab = "Heating", ylab = "Frequency")
barplot(table(train$HeatingQC), main = "HeatingQC Bar Chart", xlab = "HeatingQC", ylab = "Frequency")
barplot(table(train$CentralAir), main = "CentralAir Bar Chart", xlab = "CentralAir", ylab = "Frequency")
barplot(table(train$Electrical), main = "Electrical Bar Chart", xlab = "Electrical", ylab = "Frequency")
barplot(table(train$KitchenQual), main = "KitchenQual Bar Chart", xlab = "KitchenQual", ylab = "Frequency")
barplot(table(train$Functional), main = "Functional Bar Chart", xlab = "Functional", ylab = "Frequency")
barplot(table(train$FireplaceQu), main = "FireplaceQu Bar Chart", xlab = "FireplaceQu", ylab = "Frequency")
barplot(table(train$GarageType), main = "GarageType Bar Chart", xlab = "GarageType", ylab = "Frequency")
barplot(table(train$GarageFinish), main = "GarageFinish Bar Chart", xlab = "GarageFinish", ylab = "Frequency")
barplot(table(train$GarageQual), main = "GarageQual Bar Chart", xlab = "GarageQual", ylab = "Frequency")
barplot(table(train$GarageCond), main = "GarageCond Bar Chart", xlab = "GarageCond", ylab = "Frequency")
barplot(table(train$PavedDrive), main = "PavedDrive Bar Chart", xlab = "PavedDrive", ylab = "Frequency")
barplot(table(train$PoolQC), main = "PoolQC Bar Chart", xlab = "PoolQC", ylab = "Frequency")
barplot(table(train$Fence), main = "Fence Bar Chart", xlab = "Fence", ylab = "Frequency")
barplot(table(train$MiscFeature), main = "MiscFeature Bar Chart", xlab = "MiscFeature", ylab = "Frequency")
barplot(table(train$SaleType), main = "SaleType Bar Chart", xlab = "SaleType", ylab = "Frequency")

# Correlation matrix
par(cex = 0.6)
corrplot(cor(select_if(train, is.numeric)), type="lower", order="hclust", col = brewer.pal(n = 8, name = "Blues"))
par(cex = 1)

# -------------------------------------------------
#                More Data Cleaning
# -------------------------------------------------

# Erroneous data removal

# Errors that were found
train$MasVnrArea[train$MasVnrArea > 0 & train$MasVnrType == "None"] = 0         # Can't simply do kNN as this could still produce erroneous results
train$MasVnrType[train$MasVnrArea == 0 & train$MasVnrType != "None"] = "None"   # Opted to use "None" here and 0 on the previous line since this is the most common setup for houses in the data set
train$BsmtFinSF2[train$BsmtFinSF2 > 0 & train$BsmtFinType2 == "None"] = 0
train$`2ndFlrSF`[train$`2ndFlrSF` > 0 & train$HouseStyle == "1Story"] = 0

# Errors that could exist
train$TotalBsmtSF[train$TotalBsmtSF > 0 & train$BsmtQual == "NB"] = 0
train$BsmtUnfSF[train$BsmtUnfSF > 0 & train$BsmtQual == "NB"] = 0
train$BsmtFinSF1[train$BsmtFinSF1 > 0 & train$BsmtFinType1 == "None"] = 0
train$BsmtFinType1[train$BsmtFinSF1 == 0 & train$BsmtFinType1 != "None"] = "None"
train$BsmtFinType2[train$BsmtFinSF2 == 0 & train$BsmtFinType2 != "None"] = "None"
train$BsmtFullBath[train$BsmtFullBath > 0 & train$BsmtQual == "NB"] = 0
train$BsmtHalfBath[train$BsmtHalfBath > 0 & train$BsmtQual == "NB"] = 0
train$GarageCars[train$GarageCars > 0 & train$GarageQual == "NG"] = 0
train$GarageArea[train$GarageArea > 0 & train$GarageQual == "NG"] = 0
train$PoolArea[train$PoolArea > 0 & train$PoolQC == "NP"] = 0
train$PoolQC[train$PoolArea == 0 & train$PoolQC != "NP"] = "NP"

# Then, do the same for the test set
test$MasVnrArea[test$MasVnrArea > 0 & test$MasVnrType == "None"] = 0
test$MasVnrType[test$MasVnrArea == 0 & test$MasVnrType != "None"] = "None"
test$BsmtFinSF1[test$BsmtFinSF1 > 0 & test$BsmtFinType1 == "None"] = 0
test$BsmtFinType1[test$BsmtFinSF1 == 0 & test$BsmtFinType1 != "None"] = "None"
test$BsmtFinSF2[test$BsmtFinSF2 > 0 & test$BsmtFinType2 == "None"] = 0
test$BsmtFinType2[test$BsmtFinSF2 == 0 & test$BsmtFinType2 != "None"] = "None"
test$TotalBsmtSF[test$TotalBsmtSF > 0 & test$BsmtQual == "NB"] = 0
test$BsmtUnfSF[test$BsmtUnfSF > 0 & test$BsmtQual == "NB"] = 0
test$BsmtFullBath[test$BsmtFullBath > 0 & test$BsmtQual == "NB"] = 0
test$BsmtHalfBath[test$BsmtHalfBath > 0 & test$BsmtQual == "NB"] = 0
test$GarageCars[test$GarageCars > 0 & test$GarageQual == "NG"] = 0
test$GarageArea[test$GarageArea > 0 & test$GarageQual == "NG"] = 0
test$`2ndFlrSF`[test$`2ndFlrSF` > 0 & test$HouseStyle == "1Story"] = 0
test$PoolArea[test$PoolArea > 0 & test$PoolQC == "NP"] = 0
test$PoolQC[test$PoolArea == 0 & test$PoolQC != "NP"] = "NP"

# -------------------------------------------------
#                     Modeling
# -------------------------------------------------
