# XGBoost model
#install.packages("xgboost")
library(xgboost)
#install.packages("methods")
library(methods)
#install.packages("data.table")
library(data.table)
#install.packages("magrittr")
library(magrittr)
library(AppliedPredictiveModeling)
library(caret)
library(lubridate)
#install.packages("DiagrammeR")
library(DiagrammeR)
#install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
#install.packages("Metrics")
library(Metrics)


trainMatrix <- as.matrix(sapply(trainSFO_2, as.numeric))
testMatrix <- as.matrix(sapply(testSFO_2, as.numeric))

NumericResponseTrain<-as.numeric(trainSFO_1$Category)
length(NumericResponseTrain)
NumericResponseTest <- as.numeric(testSFO_1$Category)
length(NumericResponseTest)

numberOfClasses <- max(NumericResponseTrain)+1

param <- list("objective" = "multi:softmax",
              "eval_metric" = "merror",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3

nrow(trainMatrix)


bst.cv = xgb.cv(param=param, data = trainMatrix, label = trainSFO_1$Category,nfold = cv.nfold, nrounds = cv.nround)

nround = 50
bst = xgboost(param=param, data = trainMatrix, label = trainSFO_1$Category, nrounds=nround)

model <- xgb.dump(bst, with.stats = T)
model[1:10]

head(trainMatrix)
names <- dimnames(trainMatrix)[[2]]
names

importance_matrix <- xgb.importance(as.character(names), model = bst)
head(importance_matrix)

xgb.plot.importance(importance_matrix[,])

importance_matrix


xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)

TestPrediction<-predict(bst,testMatrix)
head(TestPrediction)
# Capturing the difference between Prediction and NumericResponse


XGBoostRmse<-RMSE(TestPrediction,NumericResponseTest)
head(XGBoostRmse)
head(as.numeric(dfTestSFKnnResponse1))

OutPutXGBoost <- c(TestPrediction,NumericResponseTest )

XGBoostPR <- postResample(pred=TestPrediction, obs=NumericResponseTest)

XGBoostPR

# Applying on the kaggle test set.
crimeSFOTest <- read.csv("data/test.csv")

t <- Sys.time(); crimeSFOTest_1 = split_date(crimeSFOTest); Sys.time()-t
crimeSFOTest_2 <- make_training_factors(crimeSFOTest_1)
testXGBMatrix <- as.matrix(sapply(crimeSFOTest_2, as.numeric))

TestPredictionXGB <- predict(bst,crimeSFOTest_2)

TestPredictionXGB$id = crimeSFOTest_1$id
write.csv(TestPredictionXGB, 'test_pred_knn_numerical_only_benchmark.csv', row.names = F)