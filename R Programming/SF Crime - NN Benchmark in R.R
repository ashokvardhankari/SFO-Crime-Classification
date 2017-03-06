# This script creates and evaluates a neural network (NN) in R for the 'San Francisco Crime Classification' competition.
# This iteration is only to get the model up and running, so there is minimal feature engineering and parameter tuning.

# Load packages.
library(data.table)
library(nnet)
library(e1071)

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
  
  # All inputs into NN must be numeric, so I convert categorical variables ('day_of_week' and 'pd_district') into dummy variables.
  # This must be done for training and test data.
  
      # Create data table with categorical variables converted to dummy variable form.
      train_dummies = data.table(model.matrix(category_predict ~ day_of_week + pd_district + x + y + hour, 
                                              data = train))
      test_dummies = data.table(model.matrix(~ day_of_week + pd_district + x + y + hour, 
                                             data = test))
      
      # Remove intercept column.
      train_dummies$`(Intercept)` = NULL
      test_dummies$`(Intercept)` = NULL
      
      # Add 'category_predict' to 'train_dummies'.
      train_dummies$category_predict = train$category_predict

  # Create data subset to train NN on (training on full data set is too time-intensive).
  # 'subset' contains at least 1 example of every crime category.
  subset = train_dummies[c(1:22000, 22032, 25861, 30024, 33954, 37298, 41980, 49306, 53715, 93717, 102637, 102645, 102678, 107734, 148476, 148476, 192191, 205046, 252094, 279792, 316491, 317527, 332821, 337881)]

#####
# CREATE MODEL AND PREDICTIONS.
  
  # Set seed to ensure reproducibility.
  set.seed(1)

  # Define model with dummy variables.
  model = category_predict ~ day_of_weekMonday + day_of_weekSaturday + day_of_weekSunday + day_of_weekThursday + day_of_weekTuesday + day_of_weekWednesday + 
    pd_districtCENTRAL + pd_districtINGLESIDE + pd_districtMISSION + pd_districtNORTHERN + pd_districtPARK + pd_districtRICHMOND + pd_districtSOUTHERN + pd_districtTARAVAL + pd_districtTENDERLOIN
  
  # Create NN.
  nn = nnet(model, data = subset, size = 2)

  # Generate predictions for training and test data.
  # Training predictions are computed for 'train' (even though model training was done using 'subset' due to time constraints).
  # For the training data, I only want to compute accuracy.
  # For the test data, I need to put predictions in a specific format for submission, as specified by Kaggle.com.
  train_pred = data.table(predict(nn,
                               newdata = train_dummies,
                               type = 'class'))
  test_pred = data.table(predict(nn, 
                                 newdata = test_dummies, 
                                 type = 'raw'))

#####
# CHECK TRAINING SET ACCURACY.
  
  # Add training set predictions to 'train'.
  train$pred = train_pred$V1
  
  # View training accuracy.
  print(table(train$category_predict == train$pred))
  print(prop.table(table(train$category_predict == train$pred)))
    
#####
# CROSS VALIDATION.
    
  # Create 'tune.control' object.
  # Certain crime categories don't have many occurences in 'subset' so I keep the number of cross validation partitions low.
  tune_control = tune.control(cross = 2)
  
  # Execute cross validation.
  t = tune.nnet(model, 
                data = subset, 
                size = 2, 
                tunecontrol = tune_control)
  
  # View estimated test set error computed by cross validation.
  print(t)

#####
# EXPORT TEST SET PREDICTIONS.

  # Predictions must be formatted as specified on Kaggle.com.
  # This is done for test data only.
  
    # Add 'test$id' to 'test_pred'.
    test_pred$id = test$id
  
  # Create csv file of test predictions.
  # This is commented out for now, since I don't actually want to create a csv.
  # write.csv(test_pred, 'test_pred_nn_benchmark.csv', row.names = F)