# This script creates and evaluates a K-Nearest-Neighbor model in R for the 'San Francisco Crime Classification' competition.
# The package used in this script cannot handle categorical variables.
# This iteration is only to get the model up and running, so there is minimal feature engineering and parameter tuning.

# Load packages.
library(data.table)
library(kknn)

# Import data.
train = data.table(read.csv('../input/train.csv',
                            header = T))
test = data.table(read.csv('../input/test.csv', 
                           header = T))

#####
# PRE-PROCESSING AND FEATURE ENGINEERING.
  
  # Rename columns of testing and training data set.
  setnames(train, names(train), c('date', 'category_predict', 'description_ignore', 'day_of_week', 'pd_district', 'resolution', 'address', 'x', 'y'))
  setnames(test, names(test), c('id', 'date', 'day_of_week', 'pd_district', 'address', 'x', 'y'))
  
  # Get hour of each crime.
  train$hour = as.numeric(substr(train$date, 12, 13))
  test$hour = as.numeric(substr(test$date, 12, 13))
  
  # Scale dependent variables in 'train'.
  x_train_scaled = scale(train$x)
  y_train_scaled = scale(train$y)
  hour_train_scaled = scale(train$hour)
  
  # Scale dependent variables in 'test' using mean and standard deviation derived from scaling variables in 'train'.
  x_test_scaled = (test$x - attr(x_train_scaled, 'scaled:center')) / attr(x_train_scaled, 'scaled:scale')
  y_test_scaled = (test$y - attr(y_train_scaled, 'scaled:center')) / attr(y_train_scaled, 'scaled:scale')
  hour_test_scaled = (test$hour - attr(hour_train_scaled, 'scaled:center')) / attr(hour_train_scaled, 'scaled:scale')

  # Create 'train_model' and 'test_model' which only include variables used in the model.
  train_model = data.table(category_predict = train$category_predict, 
                           x_scaled = x_train_scaled, 
                           y_scaled = y_train_scaled, 
                           hour_scaled = hour_train_scaled)
  setnames(train_model, 
           names(train_model), 
           c('category_predict', 'x_scaled', 'y_scaled', 'hour_scaled'))
  test_model = data.table(x_scaled = x_test_scaled, 
                          y_scaled = y_test_scaled, 
                          hour_scaled = hour_test_scaled)
    
  # Create data subset to train NN on (training on full data set is too time-intensive).
  # 'subset' contains at least 1 example of every crime category.
  subset = train_model[c(1:30000, 30024, 33954, 37298, 41097, 41980, 44479, 48707, 48837, 49306, 53715, 93717, 102637, 102645, 102678, 102919, 103517, 103712, 107734, 148476, 148476, 
                   192191, 205046, 252094, 279792, 316491, 317527, 332821, 337881)]
  
#####
# CREATE MODEL AND PREDICTIONS.
  
  # Set seed to ensure reproducibility.
  set.seed(1)
  
  # Define model.
  model = category_predict ~ x_scaled + y_scaled + hour_scaled
  
  # Create model and generate predictions for training set ('subset').
  # Variable scaling is done in this command.
  knn_train = kknn(formula = model, 
                      train = subset, 
                      test = subset, 
                      scale = T)
  
  # Create model and generate predictions for test set.
  knn_test = kknn(formula = model, 
                  train = subset,
                  test = test_model,
                  scale = T)

  # Generate predictions for training and test data.
  # Training predictions are computed for 'subset', not 'train' (due to time constraints).
  # For the training data ('subset'), I only want to compute accuracy.
  # For the test data, I need to put predictions in a specific format for submission, as specified by Kaggle.com.
  # Training and test predictions  were already computed when generating model.
  train_pred = data.table(knn_train$fitted.values)
  test_pred = data.table(knn_test$prob)

#####
# CHECK TRAINING SET ACCURACY.
# Accuracy is computed for 'subset', not 'train', due to time constraints.
  
  # Add training set predictions to 'train'.
  subset$pred = train_pred$V1
  
  # View training accuracy.
  print('Training Accuracy')
  print(table(subset$category_predict == subset$pred))
  print(prop.table(table(subset$category_predict == subset$pred)))

#####
# CROSS VALIDATION.
  
  # Conduct cross validation.
  cv = cv.kknn(model, 
               data = subset, 
               kcv = 2, 
               scale = T)
  
  # View cross validation accuracy.
  cv = data.table(cv[[1]])
  print('Cross Validation Accuracy')
  print(table(cv$y == cv$yhat))
  print(prop.table(table(cv$y == cv$yhat)))

#####
# EXPORT TEST SET PREDICTIONS.

  # Predictions must be formatted as specified on Kaggle.com.
  # This is done for test data only.
  
    # Add 'test$id' to 'test_pred'.
    test_pred$id = test$id
  
  # Create csv file of test predictions.
  # This is commented out for now, since I don't actually want to create a csv.
  # write.csv(test_pred, 'test_pred_knn_numerical_only_benchmark.csv', row.names = F)