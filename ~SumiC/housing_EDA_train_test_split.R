library(tidyverse)
library(ggforce)
library(xlsx)
library(plotly)
library(caret)
library(car)
library(MASS)


rm(list=ls())

housing = read.csv("train.csv", header = T, row.names = "Id")

#################################### Exploratory Data Analysis #########################################

View(housing)

head(housing, 10)

housing[housing == ""] <- NA
View(housing)

sum(is.na(housing))

missing = colnames(housing)[colSums(is.na(housing)) > 0]

# columns missing
ncol(housing[,missing])

missing

# column proportions missing
missing.df = (housing)[colSums(is.na(housing)) > 0]

apply(missing.df, 2, function(col)sum(is.na(col)))

apply(missing.df, 2, function(col)sum(is.na(col))/length(col))

VIM::aggr(missing.df)

# complete cases analyses
sum(!complete.cases(housing))

mean(!complete.cases(housing)) * 100

# data perspective missing
prod(dim(housing))

sum(is.na(housing))
sum(is.na(housing))/prod(dim(housing))

# mice by rows and columns
mice::md.pattern(housing)

missing_correlation = as.data.frame(mice::md.pattern(housing))

write.xlsx2(missing_correlation, file = "Missing_Correlations.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

summary(housing)

sapply(housing, class)

# check the values in the selected column
as.data.frame(table(unlist(housing$Condition2)))


#graph numerical variables

housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 1) +
  geom_histogram(fill = "coral1", col = "dark gray")


housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 2) +
  geom_histogram(fill = "coral1", col = "dark gray")


housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 3) +
  geom_histogram(fill = "coral1", col = "dark gray")


housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 4) +
  geom_histogram(fill = "coral1", col = "dark gray")

housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 5) +
  geom_histogram(fill = "coral1", col = "dark gray")

housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 6) +
  geom_histogram(fill = "coral1", col = "dark gray")

housing_new %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 7) +
  geom_histogram(fill = "coral1", col = "dark gray")


# graph categorical variables

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 1) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 2) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 3) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 4) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 5) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 6) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 7) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

housing %>%
  keep(is.factor) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap_paginate(~ key, scales = "free", nrow = 2, ncol = 3, page = 8) +
  geom_bar(fill = "cornflowerblue", col = "dark gray")

which(sapply(housing, class) == 'integer')


no_miss = colnames(housing)[colSums(is.na(housing)) == 0]

housing_all_col = data.frame(housing[, no_miss])

colnames(housing_all_col)

# only integer, non missing variables
names2 = names(housing_all_col)[sapply(housing_all_col, is.numeric)]

housing_int = data.frame(housing_all_col[, names2])
colnames(housing_int)

# only factor, non missing variables
names_factor = names(housing_all_col)[sapply(housing_all_col, is.factor)]
housing_factor = data.frame(housing_all_col[, names_factor])
colnames(housing_factor)

correlation = data.frame(cor(housing_int))
View(correlation)


##################################### Split data into train versus test #################################

# split data into train and test

library(caTools)
set.seed(0)
sample = sample.split(housing,SplitRatio = 0.75)
housing = subset(housing,sample == TRUE)
test1 = subset(housing, sample == FALSE)
 
class(test)
class(housing)


##################################### Data Cleaning ##################################################

# with missing data column vectors of class factor
names_factor_all = names(housing)[sapply(housing, is.factor)]
housing_all_factor = data.frame(housing[, names_factor_all])
colnames(housing_all_factor)

# replace NAs in factor columns to "None"
housing_factor_na = as.matrix(housing_all_factor)
housing_factor_na[is.na(housing_factor_na)] = "None"
housing_factor_na = as.data.frame(housing_factor_na)

sapply(housing_factor_na, class)


View(housing_factor_na)

# with missing data column vectors of class integer
names = names(housing)[sapply(housing, is.numeric)]
housing_int2 = data.frame(housing[, names])
names

housing_int2 =
  housing_int2 %>% 
  mutate(RemodeledYrs = (YrSold - YearRemodAdd)) %>% 
  mutate(YrsAge = (YrSold - YearBuilt))


correlation2 = data.frame(cor(housing_int2))
View(correlation2)

write.xlsx2(correlation, file = "Correlations.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx2(correlation2, file = "Correlations_v2.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

# write.xlsx2(correlation2, file = "Correlations_missing.xlsx", sheetName = "Sheet1",
#             col.names = TRUE, row.names = TRUE, append = FALSE)


descript = read.csv('data_description.txt', sep = "\t")
View(descript)

write.xlsx2(descript, file = "Descriptions.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)

housing_price = subset(housing_int2, select = SalePrice)

housing_int2 = subset(housing_int2, select = -SalePrice)
View(housing_price)

housing_price$SalePrice <- lapply(housing_price$SalePrice, log)
housing_price$SalePrice = unlist(housing_price$SalePrice)

housing_int2[c(1:38)] <- lapply(housing_int2[c(1:38)], sqrt)

View(housing_int2)
View(housing_new)

housing_new = cbind(housing_price, housing_int2, housing_factor_na)

sapply(housing_new, class)

summary(housing_int2)
summary(housing_new)

which(housing_new$RemodeledYrs == -1.00)[[1]]


as.data.frame(table(unlist(housing_new$BsmtHtCond)))

levels(housing_new$Condition2) = c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn")
levels(housing_new$Exterior1st) = c("AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "CmentBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "Stone", "Stucco", "VinylSd", "Wd Sdng", "Wd Shng")
levels(housing_new$BsmtQual) = c("Ex", "Fa", "Gd", "None", "Po", "TA")
levels(housing_new$BsmtCond) = c("Ex", "Fa", "Gd", "None", "Po", "TA")

housing_new$Condition1 = as.character(housing_new$Condition1)
housing_new$Condition2 = as.character(housing_new$Condition2)

housing_new$Exterior1st = as.character(housing_new$Exterior1st)
housing_new$Exterior2nd = as.character(housing_new$Exterior2nd)

housing_new$BsmtQual = as.character(housing_new$BsmtQual)
housing_new$BsmtCond = as.character(housing_new$BsmtCond)


housing_new = housing_new %>% 
  mutate(Condition = ifelse(Condition1 == Condition2, Condition1, paste0(Condition1, "_", Condition2)))

housing_new = housing_new %>% 
  mutate(Exterior = ifelse(Exterior1st == Exterior2nd, Exterior1st, paste0(Exterior1st, "_", Exterior2nd)))


housing_new = housing_new %>% 
  mutate(BsmtHtCond = paste0(BsmtQual, "_", BsmtCond))

sapply(housing_new, class)

housing_new$Condition = as.factor(housing_new$Condition)
housing_new$Exterior = as.factor(housing_new$Exterior)
housing_new$BsmtHtCond = as.factor(housing_new$BsmtHtCond)


head(housing_new)
View(housing_new)


housing_new = housing_new[, !(colnames(housing_new) %in% c("YrSold", "YearBuilt", "YearRemodAdd", "GarageYrBlt", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF","X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "BsmtFinType1", "BsmtFinType2", "GarageFinish", "GarageQual", "GarageCond", "MiscFeature", "Condition1", "Condition2", "Exterior1st", "Exterior2nd", "BsmtQual", "BsmtCond"))]


# imputation

sqrt(nrow(housing_new)) #Determining K for the housing dataset.


housing_temp = subset(housing_new, select = -SalePrice)


pre.test = preProcess(housing_temp, method = 'knnImpute', k=38)
housing_temp = predict(pre.test, housing_temp)

housing_new2 = cbind(housing_price, housing_temp)


View(housing_new2)


# correlations of integer variables in final data set
housing_tf = cbind(housing_price, housing_int2)
pre.int2 = preProcess(housing_tf, method = 'knnImpute', k=38)
housing_tf = predict(pre.int2, housing_tf)


correlation_tf = data.frame(cor(housing_tf))
View(correlation_tf)

write.xlsx2(correlation_tf, file = "Correlation_tf.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)


# Multiple Linear Regression

model.saturated = lm(SalePrice ~ ., data = housing_new2)

summary(model.saturated)

plot(model.saturated)
influencePlot(model.saturated)

vif(model.saturated)

# finding the linearly dependent variables (note: after some data transformations, the linearly dependent variables were successfully elminated as shown by ld.vars below)
alias(model.saturated)

ld.vars <- attributes(alias(model.saturated)$Complete)$dimnames[[1]]
ld.vars



library(broom)
write.csv(tidy(model.saturated), "Regression2.csv")

vif(model.saturated)

sapply(housing_new2, class)

avPlots(model.saturated)

write.csv(tidy(vif(model.saturated)), "VIF_revised.csv")

# remove high VIF valued variables, i.e., GVIF > 2

model.2 = lm(SalePrice ~ . - FullBath - BldgType - KitchenAbvGr - OverallQual - PoolQC - TotRmsAbvGrd - GarageArea - Fireplaces - GarageCars - BsmtUnfSF - X1stFlrSF - BsmtFinSF1 - YrsAge - X2ndFlrSF - MSSubClass - PoolArea, data = housing_new2)

summary(model.2)

plot(model.2)
influencePlot(model.2)

vif(model.2)

# the anova test shows that the reduced model is significantly different from the full model; needs further analysis; next will do a BIC and AIC test; and then stepwise regression
anova(model.2, model.saturated)

model.empty = lm(SalePrice ~ 1, data = housing_new2)
scope = list(lower = formula(model.empty), upper = formula(model.saturated))
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)

forward.1 = lm(SalePrice ~ OverallQual + Neighborhood + X1stFlrSF + X2ndFlrSF + 
  OverallCond + BsmtFinSF1 + RoofMatl + YrsAge + LotArea + 
  Condition + GarageArea + MSZoning + Functional + BsmtHtCond + 
  Fireplaces + KitchenQual + SaleCondition + CentralAir + LandSlope + 
  BsmtExposure + Heating + ScreenPorch + HeatingQC + GarageType + 
  PoolQC + MSSubClass + Foundation + BsmtFullBath + LotConfig + 
  WoodDeckSF + HalfBath + FullBath + GarageCars + EnclosedPorch + 
  LotFrontage + LowQualFinSF + Utilities + BsmtFinSF2 + BsmtUnfSF + 
  HouseStyle + PoolArea + SaleType + Electrical + RemodeledYrs, data = housing_new2)


summary(forward.1)
plot(forward.1)
influencePlot(forward.1)

vif(forward.1)

write.csv(tidy(forward.1), "Regression3.csv")

write.csv(tidy(vif(forward.1)), "VIF_v4.csv")


# removed high VIF variables after Forward regression
forward.2 = lm(SalePrice ~ Condition + ScreenPorch + Utilities + LotConfig + Functional	+ Heating	+ RoofMatl	+ WoodDeckSF + HeatingQC	+ Electrical	+ EnclosedPorch	+ Neighborhood	+ LandSlope	+ BsmtFinSF2	+ GarageType	+ SaleType	+ KitchenQual + Fireplaces	+ Foundation	+ BsmtHtCond	+ BsmtFullBath	+ LowQualFinSF	+ HouseStyle	+ CentralAir	+ OverallCond	+ MSZoning +	SaleCondition	+ HalfBath	+ LotFrontage	+ FullBath	+ LotArea	+ BsmtExposure	+ MSSubClass	+ RemodeledYrs	+ OverallQual, data = housing_new2)

summary(forward.2)
plot(forward.2)
influencePlot(forward.2)


sqrt(mean(forward.2$residuals^2))

anova(forward.2, forward.1) # model forward.1 has a significantly better fit over forward.2

# removed influential points
forward.1_influen = lm(SalePrice ~ OverallQual + Neighborhood + X1stFlrSF + X2ndFlrSF + 
                         OverallCond + BsmtFinSF1 + RoofMatl + YrsAge + LotArea + 
                         Condition + GarageArea + MSZoning + Functional + BsmtHtCond + 
                         Fireplaces + KitchenQual + SaleCondition + CentralAir + LandSlope + 
                         BsmtExposure + Heating + ScreenPorch + HeatingQC + GarageType + 
                         PoolQC + MSSubClass + Foundation + BsmtFullBath + LotConfig + 
                         WoodDeckSF + HalfBath + FullBath + GarageCars + EnclosedPorch + 
                         LotFrontage + LowQualFinSF + Utilities + BsmtFinSF2 + BsmtUnfSF + 
                         HouseStyle + PoolArea + SaleType + Electrical + RemodeledYrs, data = housing_new2[!row.names(housing_new2) %in% c("524", "826", "463", "589", "633", "969", "971", "1132", "1325", "1388", "186", "712", "1433", "1454", "411", "558", "711"), ])

summary(forward.1_influen)
plot(forward.1_influen)
influencePlot(forward.1_influen)



############## Test Model based on a reduced training set (number of obersvations split with test) ################
model.empty = lm(SalePrice ~ 1, data = housing_new2)
model.full = lm(SalePrice ~ ., data = housing_new2)
scope = list(lower = formula(model.empty), upper = formula(model.full))
forwardAIC = step(model.empty, scope, direction = "forward", k = log(1460))


forward.1 = lm(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF + 
                 OverallCond + YrsAge + LotArea + BsmtFullBath + GarageArea + 
                 MSZoning + BsmtHtCond + KitchenAbvGr + Functional + Fireplaces + 
                 Condition + Heating + ScreenPorch + SaleCondition + KitchenQual + 
                 WoodDeckSF + LandSlope + CentralAir + BsmtExposure + Street + 
                 Foundation + RemodeledYrs + PoolArea + RoofMatl, data = housing_new2)


summary(forward.1)
plot(forward.1)
influencePlot(forward.1)

vif(forward.1)



##################### Test Data from sampling of the original train data ##############################################

# read in the test data set
# test = read.csv("test.csv", header = T, row.names = "Id")

test = test1

test[test == ""] <- NA

sum(is.na(test))

missing_test = colnames(test)[colSums(is.na(test)) > 0]

# columns missing
ncol(test[,missing_test])

missing_test


# with missing data column vectors of class factor
names_factor_test = names(test)[sapply(test, is.factor)]
test_all_factor = data.frame(test[, names_factor_test])
colnames(test_all_factor)

# replace NAs in factor columns to "None"
test_factor_na = as.matrix(test_all_factor)
test_factor_na[is.na(test_factor_na)] = "None"
test_factor_na = as.data.frame(test_factor_na)

sapply(test_factor_na, class)


# with missing data column vectors of class integer
test_names = names(test)[sapply(test, is.numeric)]
test_int2 = data.frame(test[, test_names])
test_names

test_int2 =
  test_int2 %>% 
  mutate(RemodeledYrs = (YrSold - YearRemodAdd)) %>% 
  mutate(YrsAge = (YrSold - YearBuilt))

test_price = subset(test_int2, select = SalePrice)

test_int2 = subset(test_int2, select = -SalePrice)
View(test_price)

test_price$SalePrice <- lapply(test_price$SalePrice, log)
test_price$SalePrice = unlist(test_price$SalePrice)


test_int2[c(1:38)] <- lapply(test_int2[c(1:38)], sqrt)

summary(test_int2)
View(test_int2)


# test_new = cbind(test_int2, test_factor_na)
test_new = cbind(test_price, test_int2, test_factor_na)

sapply(test_new2, class)
summary(test_new)


levels(test_new$Exterior1st)[levels(test_new$Exterior1st) == "BrkComm"] = "Brk Cmn"
levels(test_new$Exterior1st)[levels(test_new$Exterior1st) == "CemntBd"] = "CmentBd"

levels(test_new$Condition2) = c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn")
levels(test_new$Exterior1st) = c("AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "CmentBd", "HdBoard", "ImStucc", "MetalSd", "None","Other", "Plywood", "Stone", "Stucco", "VinylSd", "Wd Sdng", "Wd Shng")
levels(test_new$BsmtQual) = c("Ex", "Fa", "Gd", "None", "Po", "TA")
levels(test_new$BsmtCond) = c("Ex", "Fa", "Gd", "None", "Po", "TA")

test_new$Condition1 = as.character(test_new$Condition1)
test_new$Condition2 = as.character(test_new$Condition2)

test_new$Exterior1st = as.character(test_new$Exterior1st)
test_new$Exterior2nd = as.character(test_new$Exterior2nd)

test_new$BsmtQual = as.character(test_new$BsmtQual)
test_new$BsmtCond = as.character(test_new$BsmtCond)


test_new = test_new %>% 
  mutate(Condition = ifelse(Condition1 == Condition2, Condition1, paste0(Condition1, "_", Condition2)))

test_new = test_new %>% 
  mutate(Exterior = ifelse(Exterior1st == Exterior2nd, Exterior1st, paste0(Exterior1st, "_", Exterior2nd)))

# test_new = test_new %>% 
#   mutate(BsmtHtCond = ifelse(BsmtQual == BsmtCond, BsmtQual, paste0(BsmtQual, "_", BsmtCond)))

test_new = test_new %>% 
  mutate(BsmtHtCond = paste0(BsmtQual, "_", BsmtCond))


sapply(test_new, class)
sapply(housing_new2, class)


test_new$Condition = as.factor(test_new$Condition)
test_new$Exterior = as.factor(test_new$Exterior)
test_new$BsmtHtCond = as.factor(test_new$BsmtHtCond)

test_new2 = predict(pre.test, test_new)
summary(test_new2)
summary(housing_new2)


forward.test = lm(SalePrice ~ OverallQual + GrLivArea + Neighborhood + TotalBsmtSF + 
                 OverallCond + YrsAge + LotArea + BsmtFullBath + GarageArea + 
                 MSZoning + BsmtHtCond + KitchenAbvGr + Functional + Fireplaces + 
                 Condition + Heating + ScreenPorch + SaleCondition + KitchenQual + 
                 WoodDeckSF + LandSlope + CentralAir + BsmtExposure + Street + 
                 Foundation + RemodeledYrs + PoolArea + RoofMatl, data = test_new2)

summary(forward.test)

summary(forward.1)


################################### Test on unknown data - Sales Price not given ############################

# read in the test data set
test = read.csv("test.csv", header = T, row.names = "Id")

test[test == ""] <- NA

sum(is.na(test))

missing_test = colnames(test)[colSums(is.na(test)) > 0]

# columns missing
ncol(test[,missing_test])

missing_test


# with missing data column vectors of class factor
names_factor_test = names(test)[sapply(test, is.factor)]
test_all_factor = data.frame(test[, names_factor_test])
colnames(test_all_factor)

# replace NAs in factor columns to "None"
test_factor_na = as.matrix(test_all_factor)
test_factor_na[is.na(test_factor_na)] = "None"
test_factor_na = as.data.frame(test_factor_na)

sapply(test_factor_na, class)


# with missing data column vectors of class integer
test_names = names(test)[sapply(test, is.numeric)]
test_int2 = data.frame(test[, test_names])
test_names

test_int2 =
  test_int2 %>% 
  mutate(RemodeledYrs = (YrSold - YearRemodAdd)) %>% 
  mutate(YrsAge = (YrSold - YearBuilt))


test_int2[c(1:38)] <- lapply(test_int2[c(1:38)], sqrt)

summary(test_int2)
View(test_int2)


test_new = cbind(test_int2, test_factor_na)

sapply(test_new2, class)
summary(test_new)


levels(test_new$Exterior1st)[levels(test_new$Exterior1st) == "BrkComm"] = "Brk Cmn"
levels(test_new$Exterior1st)[levels(test_new$Exterior1st) == "CemntBd"] = "CmentBd"

levels(test_new$Condition2) = c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn")
levels(test_new$Exterior1st) = c("AsbShng", "AsphShn", "Brk Cmn", "BrkFace", "CBlock", "CmentBd", "HdBoard", "ImStucc", "MetalSd", "None","Other", "Plywood", "Stone", "Stucco", "VinylSd", "Wd Sdng", "Wd Shng")
levels(test_new$BsmtQual) = c("Ex", "Fa", "Gd", "None", "Po", "TA")
levels(test_new$BsmtCond) = c("Ex", "Fa", "Gd", "None", "Po", "TA")

test_new$Condition1 = as.character(test_new$Condition1)
test_new$Condition2 = as.character(test_new$Condition2)

test_new$Exterior1st = as.character(test_new$Exterior1st)
test_new$Exterior2nd = as.character(test_new$Exterior2nd)

test_new$BsmtQual = as.character(test_new$BsmtQual)
test_new$BsmtCond = as.character(test_new$BsmtCond)


test_new = test_new %>% 
  mutate(Condition = ifelse(Condition1 == Condition2, Condition1, paste0(Condition1, "_", Condition2)))

test_new = test_new %>% 
  mutate(Exterior = ifelse(Exterior1st == Exterior2nd, Exterior1st, paste0(Exterior1st, "_", Exterior2nd)))

# test_new = test_new %>% 
#   mutate(BsmtHtCond = ifelse(BsmtQual == BsmtCond, BsmtQual, paste0(BsmtQual, "_", BsmtCond)))

test_new = test_new %>% 
  mutate(BsmtHtCond = paste0(BsmtQual, "_", BsmtCond))


sapply(test_new, class)
sapply(housing_new2, class)


test_new$Condition = as.factor(test_new$Condition)
test_new$Exterior = as.factor(test_new$Exterior)
test_new$BsmtHtCond = as.factor(test_new$BsmtHtCond)

test_new2 = predict(pre.test, test_new)
summary(test_new2)
summary(housing_new2)


as.data.frame(table(unlist(test_new2$BsmtHtCond)))

library(forcats)
test_new2$MSZoning = test_new2$MSZoning %>% 
  fct_collapse(RL = c("None", "RL"))

test_new2$KitchenQual = test_new2$KitchenQual %>% 
  fct_collapse(TA = c("None", "TA"))

test_new2$Functional = test_new2$Functional %>% 
  fct_collapse(Typ = c("None", "Typ"))

test_new2$Condition = test_new2$Condition %>% 
  fct_collapse(Feedr_Norm = c("Feedr_Artery", "Feedr_Norm"))

test_new2$Condition = test_new2$Condition %>% 
  fct_collapse(PosN = c("PosA", "PosN"))

test_new2$Condition = test_new2$Condition %>% 
  fct_collapse(RRAn_Norm = c("RRAn_Artery", "RRAn_Norm"))

test_new2$Condition = test_new2$Condition %>% 
  fct_collapse(RRNn_Norm = c("RRNn_Artery", "RRNn_Norm"))

test_new2$Condition = test_new2$Condition %>% 
  fct_collapse(PosN_Norm = c("PosN", "PosN_Norm"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Ex_Po = c("Ex_None", "Ex_Po"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Fa_Po = c("Fa_Fa", "Fa_Po"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Gd_Ex = c("Gd_Gd", "Gd_Ex"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(None_Gd = c("None_Ex", "None_Gd"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Po_Po = c("None_Po", "Po_Po"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Po_Ex = c("Po_Gd", "Po_Ex"))

test_new2$BsmtHtCond= test_new2$BsmtHtCond %>% 
  fct_collapse(Po_Po = c("Po_None", "Po_Po"))



predict(forward.1, test_new2, interval = "confidence") #Construct confidence intervals
#for the average value of an
#outcome at a specific point.

predict(forward.1, test_new2, interval = "prediction")

y.predict = predict(forward.1, test_new2)

y.predict2 = exp(y.predict)
y.predict2

y.predict3 = as.data.frame(y.predict2)
y.predict3


write.xlsx2(y.predict3, file = "submission_v8.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)
