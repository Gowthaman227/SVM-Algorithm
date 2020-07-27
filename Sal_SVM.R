Salary_train <- read.csv(file.choose())
View(Salary_train)
Salary_train <- Salary_train[,c(ncol(Salary_train),1:(ncol(Salary_train)-1))]
View(Salary_train)
Salary_test <- read.csv(file.choose())
View(Salary_test)
Salary_test <- Salary_test[c(ncol(Salary_test),1:(ncol(Salary_test)-1))]
View(Salary_test)
## Building a SVM Model on training data
## Using Vanilladot 
library(kernlab) 
Salary_Model <- ksvm(Salary~.,data=Salary_train,kernel="vanilladot")
summary(Salary_Model)
## Prediction on testing data
Sal_test_pred <- predict(Salary_Model,Salary_test)
library(gmodels)
CrossTable(Sal_test_pred,Salary_test$Salary)
mean(Sal_test_pred==Salary_test$Salary)# Accuracy is 84.6%

## Improving model performance bu using "rbfplot"----
Salary_Model <- ksvm(Salary~.,data=Salary_train,kernel="rbf") 
summary(Salary_Model)
## Prediction on testing data
Sal_test_pred <- predict(Salary_Model,Salary_test)
CrossTable(Sal_test_pred,Salary_test$Salary)
mean(Sal_test_pred==Salary_test$Salary)## Accuracy is 85.46

## Improving model performance ----
Salary_Model <- ksvm(Salary~.,data=Salary_train,kernel="tanh")
summary(Salary_Model)
## Prediction on testing data
Sal_test_pred <- predict(Salary_Model,Salary_test)
CrossTable(Sal_test_pred,Salary_test$Salary)
mean(Sal_test_pred==Salary_test$Salary)## Accuracy is 66.38

## SVM Model with kernel function "rbf" has high accuracy can be considered.

