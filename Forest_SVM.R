For_fires <- read.csv(file.choose())
View(For_fires)
For_fires <- For_fires[c(ncol(For_fires),1:(ncol(For_fires)-1))]
View(For_fires)
attach(For_fires)
summary(For_fires)
table(For_fires$size_category)
## Creating Training and Testing Dataset
Forest_train <- For_fires[1:388,]
View(Forest_train)
Forest_test <- For_fires[389:517,]
View(Forest_test)
## Bulid a SVM Model on training dataset
## Using Linear Kernel Function
library(e1071)
Forest_Model <- svm(size_category~.,data=Forest_train,kernel="linear",scale = FALSE)
summary(Forest_Model)
## Prediction on testing testing dataset
Forest_test_pred <- predict(Forest_Model,Forest_test)
library(gmodels)
CrossTable(Forest_test_pred,Forest_test$size_category)
mean(Forest_test_pred==Forest_test$size_category)## Accuracy=96.89%

## Improving model performance by using polynomial function
Forest_Model <- svm(size_category~.,data=Forest_train,kernel="polynomial",scale = FALSE)
summary(Forest_Model)
## Prediction on testing testing dataset
Forest_test_pred <- predict(Forest_Model,Forest_test)
CrossTable(Forest_test_pred,Forest_test$size_category)
mean(Forest_test_pred==Forest_test$size_category)## Accuracy=96.12%

## Improving model performance by using radial function
Forest_Model <- svm(size_category~.,data=Forest_train,kernel="radial",scale = FALSE)
summary(Forest_Model)
## Prediction on testing testing dataset
Forest_test_pred <- predict(Forest_Model,Forest_test)
CrossTable(Forest_test_pred,Forest_test$size_category)
mean(Forest_test_pred==Forest_test$size_category)## Accuracy=69.76%

## Improving model performance by using sigmoid function
Forest_Model <- svm(size_category~.,data=Forest_train,kernel="sigmoid",scale = FALSE)
summary(Forest_Model)
## Prediction on testing testing dataset
Forest_test_pred <- predict(Forest_Model,Forest_test)
CrossTable(Forest_test_pred,Forest_test$size_category)
mean(Forest_test_pred==Forest_test$size_category)## Accuracy=69.76%

## Among Four model SVM model with linear kernel function has high accuracy


