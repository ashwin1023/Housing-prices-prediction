## Read data
data_train <- read.csv(file="train.csv",header = T)
data_test <- read.csv(file="test.csv",header = T)
#******************** Data pre processing ***************************************************

str(data_train)
str(data_test)
sapply(data_test,class) == sapply(data_train,class)
is.na(data_train)
summary(data_train)
# Checking the missing value %, if > 5%, better to remove that variable
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(data_train,2,pMiss)
# apply(data_train,1,pMiss)
# apply(data_test,2,pMiss)
# apply(data_test,1,pMiss)
# data_train <- data_train[-c(4,7,58,59,60,61,64,65,73,74,75)]
# data_test <- data_test[-c(4,7,58,59,60,61,64,65,73,74,75)]

## By backward deletion, and Multicollinearity correction, we got only following variables
# data_train <- data_train[-c(2,3,8,10,22,26,29,30,32,35,36,40,41,42,43,46,48,49,50,51,57,62,63,66,67,68,69,70,71,76,77,78,79)]
# data_test <- data_test[-c(2,3,8,10,22,26,29,30,32,35,36,40,41,42,43,46,48,49,50,51,57,62,63,66,67,68,69,70,71,76,77,78,79)]
data_train <- data_train[-c(2,3,4,7,8,10,22,26,29,30,32,35,36,40,41,42,43,46,48,49,50,51,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,73,74,75,76,77,78,79)]
data_test <- data_test[-c(2,3,4,7,8,10,22,26,29,30,32,35,36,40,41,42,43,46,48,49,50,51,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,73,74,75,76,77,78,79)]
names(data_train)
names(data_test)
str(data_train)
str(data_test)
is.na(data_train)
summary(data_train)
#data_train$TotalBsmtSF <- as.factor(data_train$TotalBsmtSF)
#data_train$TotalBsmtSF <- as.integer(data_train$TotalBsmtSF)
#data_test$TotalBsmtSF <- as.factor(data_test$TotalBsmtSF)
#data_test$TotalBsmtSF <- as.integer(data_test$TotalBsmtSF)
## Missing value Imputation
#library(mlr)
# imp1 <- impute(data = data_train,target = "SalePrice",classes = list(integer=imputeMedian(), factor=imputeMode()))
# imp2 <- impute(data = data_test,target = "SalePrice",classes = list(integer=imputeMedian(), factor=imputeMode()))
# train <- imp1$data
# test <- imp2$data

#replace NA with the median if numeric [for TRAIN DATA]
data_train$MasVnrArea[is.na(data_train$MasVnrArea)] <- median(data_train$MasVnrArea, na.rm = TRUE)
data_train$TotalBsmtSF[is.na(data_train$TotalBsmtSF)] <- median(data_train$TotalBsmtSF, na.rm = TRUE)
data_train$BsmtUnfSF[is.na(data_train$BsmtUnfSF)] <- median(data_train$BsmtUnfSF, na.rm = TRUE)
data_train$BsmtFinSF2[is.na(data_train$BsmtFinSF2)] <- median(data_train$BsmtFinSF2, na.rm = TRUE)

#replace NA with the mode if categorical [for TRAIN DATA]
data_train$BsmtQual[is.na(data_train$BsmtQual)] <- "TA"
data_train$BsmtExposure[is.na(data_train$BsmtExposure)] <- "No"
data_train$BsmtFinType1[is.na(data_train$BsmtFinType1)] <- "Unf"
data_train$Exterior1st[is.na(data_train$Exterior1st)] <- "VinylSd"
data_train$Exterior2nd[is.na(data_train$Exterior2nd)] <- "VinylSd"
data_train$KitchenQual[is.na(data_train$KitchenQual)] <- "TA"
data_train$Functional[is.na(data_train$Functional)] <- "Typ"

#replace NA with the median if numeric [For TEST DATA]
data_test$MasVnrArea[is.na(data_test$MasVnrArea)] <- median(data_test$MasVnrArea, na.rm = TRUE)
data_test$TotalBsmtSF[is.na(data_test$TotalBsmtSF)] <- median(data_test$TotalBsmtSF, na.rm = TRUE)
data_test$BsmtUnfSF[is.na(data_test$BsmtUnfSF)] <- median(data_test$BsmtUnfSF, na.rm = TRUE)
data_test$BsmtFinSF2[is.na(data_test$BsmtFinSF2)] <- median(data_test$BsmtFinSF2, na.rm = TRUE)

#replace NA with the mode if categorical [For TEST DATA]
data_test$BsmtQual[is.na(data_test$BsmtQual)] <- "TA"
data_test$BsmtExposure[is.na(data_test$BsmtExposure)] <- "No"
data_test$BsmtFinType1[is.na(data_test$BsmtFinType1)] <- "Unf"
data_test$Exterior1st[is.na(data_test$Exterior1st)] <- "VinylSd"
data_test$Exterior2nd[is.na(data_test$Exterior2nd)] <- "VinylSd"
data_test$KitchenQual[is.na(data_test$KitchenQual)] <- "TA"
data_test$Functional[is.na(data_test$Functional)] <- "Typ"

# Check whether NA's are replaced or not
table(is.na(data_train))
table(is.na(data_test))
summary(data_test)
# Checking any variables are having diff levels 
sapply(data_test,class) == sapply(data_train,class)

## Normalize the data
library(dplyr)

set.seed(1234)
data_train <- as.data.frame(data_train)
data_test <- as.data.frame(data_test)

SalePrice <- data_train$SalePrice
Id_tr <- data_test$Id
Id <- data_test$Id
library(caret)
# Removing goal class that is column 37
preObj <- preProcess(data_train[,-c(1,37)], method= "scale")
data_train_sc <- predict(preObj, data_train[,-c(1,37)])
data_train_sc
preObj1 <- preProcess(data_test[,-1], method= "scale")
data_test_sc <- predict(preObj1, data_test[,-1])
data_test_sc
data_train_sc <- cbind(data_train_sc, cbind(SalePrice,Id_tr))
data_train_sc
data_test_sc <- cbind(data_test_sc, Id)
data_test_sc
# data_train_sc <- data_train %>% mutate_each_(funs(scale(.) %>% as.vector), vars=c("LotArea","OverallQual","OverallCond","YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","GrLivArea","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","PoolArea"))

# Fit the Model
fit.lm <- lm(SalePrice ~ ., data_train_sc)
summary(fit.lm)
# vif(fit.lm)  # Error aliasing factors
# alias( lm( SalePrice ~ ., data_train ) )
data_train_sc <- data_train_sc[-17]  # -"Exterior1st" since having different levels
data_test_sc <- data_test_sc[-17]

fit.lm <- lm(SalePrice ~ ., data_train_sc)
summary(fit.lm)
# checking multicollinearity
library(car)
vif(fit.lm)
names(data_test_sc)
data_train_sc <- data_train_sc[-c(6,10,27)]  # -"Exterior1st"
data_test_sc <- data_test_sc[-c(6,10,27)]

data_train <- data_train[-c(6,10,27)]  # -"Exterior1st"
data_test <- data_test[-c(6,10,27)]

fit.lm <- lm(SalePrice ~ ., data_train_sc)
summary(fit.lm)
vif(fit.lm)
# fit.lm$xlevels$TotalBsmtSF <- union(fit.lm$xlevels$TotalBsmtSF, levels(data_test_sc$TotalBsmtSF))
pred1 = predict(fit.lm, newdata = data_test_sc, type = "response")
final = data.frame(Id = data_test_sc$Id, SalePrice = pred1)
write.csv(final, "f22.csv", row.names = F)
#library(Metrics)
#rmsle(log(data_train$SalePrice), log(pred1)


#****************** RandomForest ************************************************

library(randomForest)
fit.rf <- randomForest(SalePrice ~ ., data_train_sc)
summary(fit.rf)
pred <- predict(fit.rf, newdata = data_test_sc, type = "response")

sapply(data_test_sc,class) == sapply(data_train_sc,class)
  


table(pred, data_test$SalePrice)
predict(model, data_test)
final = data.frame(Id = data_test$Id, SalePrice = pred)
write.csv(final, "f13.csv", row.names = F, na = "163000" )

library(xgboost)

model_xgb = train(SalePrice ~ .,data = data_train_sc,method = "xgbTree",metric = "Kappa",trControl = trcntrl)
pred_xgb = predict(model_xgb, newdata = data_test_sc)
table(pred_xgb)

submission = data.frame(Day = weather_test$Day,Rain = pred_xgb)
submission = data.frame(Id = data_test$Id, SalePrice = pred_xgb)

write.csv(submission,"xgb.csv",row.names = F)




