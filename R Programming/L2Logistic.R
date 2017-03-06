


# Copying the dataset for Logistic
trainSFO_L2 <- trainSFO_1
testSFO_L2 <- testSFO_1
testSFO_L2Pred <- crimeSFOTest_1

target <- trainSFO_L2$Category

# Scale dependent variables in 'trainSFO_L2'.
x_train_scaled = scale(trainSFO_L2$X)
y_train_scaled = scale(trainSFO_L2$Y)
hour_train_scaled = scale(as.numeric(trainSFO_L2$Hour))
year_train_scaled = scale(as.numeric(trainSFO_L2$Years))
month_train_scaled = scale(as.numeric(trainSFO_L2$Month))
day_train_scaled = scale(as.numeric(trainSFO_L2$DayOfMonth))

# Scale dependent variables in 'testSFO_L2'.
x_test_scaled = scale(testSFO_L2$X)
y_test_scaled = scale(testSFO_L2$Y)
hour_test_scaled = scale(as.numeric(testSFO_L2$Hour))
year_test_scaled = scale(as.numeric(testSFO_L2$Years))
month_test_scaled = scale(as.numeric(testSFO_L2$Month))
day_test_scaled = scale(as.numeric(testSFO_L2$DayOfMonth))

# Scale dependent variables in 'testSFO_L2Pred'.
x_test_scaled = scale(testSFO_L2Pred$X)
y_test_scaled = scale(testSFO_L2Pred$Y)
hour_test_scaled = scale(as.numeric(testSFO_L2Pred$Hour))
year_test_scaled = scale(as.numeric(testSFO_L2Pred$Years))
month_test_scaled = scale(as.numeric(testSFO_L2Pred$Month))
day_test_scaled = scale(as.numeric(testSFO_L2Pred$DayOfMonth))


# Create 'train_model' and 'test_model' which only include variables used in the model.
train_model = data.table(x_scaled = x_train_scaled, 
                         y_scaled = y_train_scaled, 
                         year_scaled = year_train_scaled,
                         hour_scaled = hour_train_scaled,
                         month_scaled = month_train_scaled,
                         day_scaled = day_train_scaled)
train_model <- as.data.frame(train_model)
setnames(train_model, names(train_model), 
         c('x_scaled', 'y_scaled', 'year_scaled', 'hour_scaled', 'month_scaled', 'day_scaled' ))
head(train_model)
responseTrain <- response # Response Variable

test_model = data.table(x_scaled = x_test_scaled, 
                        y_scaled = y_test_scaled, 
                        year_scaled = year_test_scaled,
                        hour_scaled = hour_test_scaled,
                        month_scaled = month_test_scaled,
                        day_scaled = day_test_scaled)

test_modelL2 = data.table(x_scaled = x_test_scaled, 
                        y_scaled = y_test_scaled, 
                        year_scaled = year_test_scaled,
                        hour_scaled = hour_test_scaled,
                        month_scaled = month_test_scaled,
                        day_scaled = day_test_scaled)

#====================
# Testing L2 Logistic
#====================
logisticTest <- LiblineaR(train_model, response, type = 0, verbose = FALSE)

# Predicting on Test set
submit1 <- data.frame(predict(logisticTest,test_modelL2, proba = TRUE)$probabilities[, levels(target)])
write.csv(submit1, 'Ashok_submit1.csv', row.names = F)

testPredL2 <- predict(logisticTest, test_model)

predOutput1 <- as.data.frame(testSFO_L2$Category)
predOutput1$Observed <- testPredL2$predictions
colnames(predOutput1) <- c('obs', 'pred')

defaultSummary(predOutput1)

# Confusion Matrix
confMat1 <- table(testPred1$predictions, testSFO_1$Category)
print(confMat1)


logisticTest <- LiblineaR(train_model, response, type = 7, verbose = FALSE)
# Predicting on Test set
testPredL3 <- predict(logisticTest, test_model)

predOutput2 <- as.data.frame(testSFO_L2$Category)
predOutput2$Observed <- testPredL3$predictions
colnames(predOutput2) <- c('obs', 'pred')
head(predOutput2)

defaultSummary(predOutput2)




#-----------------------------
# GBM
#-----------------------------

library(gbm)

fitControl<- trainControl(method="repeatedCV",
                          number=5,
                          repeats=5,
                          allowParallel = T,
                          predictionBounds = c(1,3))

ntrees = 10
gbmSFOFit <- gbm.fit(x = train_model, y = response, distribution = "multinomial",  n.trees = ntrees, 
                     shrinkage = 0.01, interaction.depth = 3, verbose = TRUE)

summary(gbmSFOFit)
gbm.perf(gbmSFOFit)

# Look at the effects of each variable
for(i in 1:length(gbmSFOFit$var.names)){
      plot(gbmSFOFit, i.var = i,
           ntrees = gbm.perf(gbmSFOFit, plot.it = FALSE),
           type = "response"
           )
}

# Predicting on test set
gbmSFOTestFit <- predict(object = gbmSFOFit, newdata = test_model,
                         n.trees = gbm.perf(gbmSFOFit, plot.it = FALSE),
                         type = "response")

gbmSFOTestFitSub <- predict(object = gbmSFOFit, newdata = test_modelL2,
                         n.trees = gbm.perf(gbmSFOFit, plot.it = FALSE),
                         type = "response")

submit1 <- data.frame(predict(gbmSFOFit,test_modelL2, n.trees = gbm.perf(gbmSFOFit, plot.it = FALSE),
                              proba = TRUE)$probabilities[, levels(target)])

# With Cross Validation
gbmModel <- train(x = train_model, y = response, method="gbm", 
                  trControl = fitControl, verbose=TRUE)
gbmModel
