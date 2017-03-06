################################################################################################                     
                        # Anusha Ganti/ Ashok Vardhan Kari
                        # CS 584 - Spring 2016
                        # San Francisco Crime Classification (Final Project)
################################################################################################

# Installing all the required packages (Run if these packages were not installed before)
install.packages("ggmap")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("caret")
install.packages("e1071")
install.packages("dbscan")
install.packages("MASS")
install.packages("ggExtra")
install.packages("Liblinear")
install.packages("readr")
install.packages("lubridate")
install.packages("googleVis")
install.packages("AppliedPredictiveModeling")
install.packages("nnet")
install.packages("klaR")
install.packages("glmnet")
install.packages("data.table")
install.packages("knn")
install.packages("xgboost")
install.packages("methods")
install.packages("magrittr")
install.packages("DiagrammeR")
install.packages("Ckmeans.1d.dp")
install.packages("Metrics")


# Loading all the required packages
library(ggmap)
library(methods)
library(magrittr)
library(Metrics)
library(Ckmeans.1d.dp)
library(DiagrammeR)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(e1071)
library(dbscan)
library(MASS)
library(ggExtra)
library(LiblineaR)
library(readr)
library(lubridate)
library(googleVis)
library(AppliedPredictiveModeling)
library(nnet)
library(klaR)
library(glmnet)
library(data.table)
library(kknn)
library(xgboost)

#----------------------------------------
# Reading data
#----------------------------------------

      # Reading the dataset
      crimeSFO <- read.csv("data/train.csv")
      
      ## Splitting dataset into training (75%) and testing (25%)
      samp_size <- floor(0.75 * nrow(crimeSFO))
      
      ## set the seed to make your partition reproductible
      set.seed(120)
      train_index <- sample(seq_len(nrow(crimeSFO)), size = samp_size)
      trainSFO <- crimeSFO[train_index, ]
      testSFO <- crimeSFO[-train_index, ]
      
      # Downloading SFO map to plot crimes recorded from 2003-2015
      map <- get_map(location="sanfrancisco",zoom=12,source="osm")
      plot(map)
      
#------------------------------------
# Creating new Variables
#------------------------------------
      
      # Splitting the timestsamp and address predictors
      split_date <- function(crimeSFO) {
            crimeSFO$Years = strftime(strptime(crimeSFO$Dates, "%Y-%m-%d %H:%M:%S"),"%Y")
            crimeSFO$Month = strftime(strptime(crimeSFO$Dates, "%Y-%m-%d %H:%M:%S"),"%m")
            crimeSFO$DayOfMonth = strftime(strptime(crimeSFO$Dates, "%Y-%m-%d %H:%M:%S"),"%d")
            crimeSFO$Hour = strftime(strptime(crimeSFO$Dates, "%Y-%m-%d %H:%M:%S"),"%H")
            crimeSFO$YearsMo = paste( crimeSFO$Years, crimeSFO$Month, sep = "-" )
            
            crimeSFO$DayOfWeek = factor(crimeSFO$DayOfWeek,
                                        levels=c("Monday","Tuesday","Wednesday","Thursday",
                                                 "Friday","Saturday","Sunday"), ordered=TRUE)
            
            crimeSFO$weekday = "Weekday"
            crimeSFO$weekday[crimeSFO$DayOfWeek== "Saturday" | 
                                   crimeSFO$DayOfWeek== "Sunday" | 
                                   crimeSFO$DayOfWeek== "Friday" ] = "Weekend"
            
            addr_spl = strsplit(as.character(crimeSFO$Address),"/")
            crimeSFO$AddressType = "Non-Intersection"
            ind_l = vector()
            ind_inxn = sapply(1:dim(crimeSFO)[1], 
                              function(x) length(addr_spl[[x]]) == 2)
            crimeSFO$AddressType[ ind_inxn ]="Intersection"
            return(crimeSFO)
      }
      
      # Creating binary predictors from categorical predictors 
      make_training_factors = function(df) {
            df$Years=paste("Yr",df$Years,sep = ".")
            df$Years = factor(df$Years)
            y <- as.data.frame(model.matrix(~df$Years - 1))
            names(y) <- levels(df$Years)
            
            df$Hour=paste("Hr",df$Hour,sep = ".")
            df$Hour = factor(df$Hour)
            h <- as.data.frame(model.matrix(~df$Hour - 1))
            names(h) <- levels(df$Hour)
            
            dow <- as.data.frame(model.matrix(~df$DayOfWeek - 1))
            names(dow) <- levels(df$DayOfWeek)
            
            df$Month=paste("Mon",df$Month,sep = ".")
            df$Month = factor(df$Month)
            m <- as.data.frame(model.matrix(~df$Month-1))
            names(m) <- levels(df$Month)
            head(m)
            
            district <- as.data.frame(model.matrix(~df$PdDistrict - 1))
            names(district) <- levels(df$PdDistrict)
            
            df$pY=paste(df$PdDistrict,df$Years,sep = ".")
            df$pY = factor(df$pY)
            pY <- as.data.frame(model.matrix(~df$pY - 1))
            names(pY) <- levels(df$pY)
            #training set
            train <- data.frame( y,dow, h, district, m,pY)
            return(train)
      }
      
      # Creating all date and related variables
      t <- Sys.time(); trainSFO_1 = split_date(trainSFO); Sys.time()-t
      t <- Sys.time(); testSFO_1 = split_date(testSFO); Sys.time()-t
    
      
#------------------------------------------
# Exploratory Data Analysis
#------------------------------------------
      
      # Types of crimes and its count in a bar plot
      data_plot = trainSFO_1 %>%
            group_by(Category) %>%
            summarise(count = n()) %>%
            transform(Category = reorder(Category,-count))
      
      ggplot(data_plot) + 
            geom_bar(aes(x=Category, y=count, color = Category, fill = Category), stat="identity")+
            coord_flip()+ theme(legend.position="None")+
            ggtitle("Number of crimes in individual category")+
            xlab("Category of crime")+
            ylab("Number of crimes")+ ylim(0,150000)
      
      ## Crime category showed by different break-downs
      
      # Crime by Day Of Week
      months_name = c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")
      
      data_plot = trainSFO_1 %>%
            group_by(DayOfWeek,Years,Month,YearsMo) %>%
            summarise(count = n()) 
      p1 = ggplot(data = data_plot,aes(x=DayOfWeek, y=count,fill = DayOfWeek)) + 
            geom_boxplot() + 
            theme(legend.position="None") +
            xlab("Day of week")+
            ylab("Number of crime incidents")+
            coord_cartesian(ylim = c(300,1200))
      
      plot(p1)
      
      # Crime vs month
      data_plot = trainSFO_1 %>%
            group_by(Month,Years,Month,YearsMo) %>%
            summarise(count = n()) 
      p2 = ggplot(data = data_plot,aes(x=as.numeric(Month), y=count,fill = Month)) + 
            geom_boxplot() + 
            xlab("Month")+
            ylab("Number of crime incidents")+
            theme(legend.position="None") +
            scale_x_continuous(breaks = 1:12, labels=months_name)
      
      plot(p2)
      
      # Crime vs hour of the day
      data_plot = trainSFO_1 %>%
            group_by(Hour,Years,Month,YearsMo) %>%
            summarise(count = n()) 
      p3 = ggplot(data = data_plot,aes(x=Hour, y=count,fill = Hour)) + 
            geom_boxplot() + 
            xlab("Hour of day")+
            ylab("Number of crime incidents")+
            theme(legend.position="None")
      
      plot(p3)
      
      grid.arrange(p1,p2,p3,ncol = 1,top="Crime rates by Day of Week, Month, Hour of Day")
      
      # Google Vis (Visualzation in motion charts using "googleVis" package)
      crime_Density <- as.data.frame(table(trainSFO$Category))
      data_plot2 <- as.data.frame(data_plot2)
      write.csv(data_plot2, file = 'crime_year.csv')
      
      # Visualizing Crime Density over the years
      crime_Density <- read.csv("crime_year.csv", header = TRUE)
      head(crime_Density)
      crime_den = gvisMotionChart(crime_Density, 
                                  idvar="Category", 
                                  timevar="Years",
                                  options = list(width=1300, height=800,  title='Crime Density Over the Years'))
      
      plot(crime_den)
      
#---------------------------------------------------------------------------------------------------------------------
                                    # Classification Algorithms (Linear)
#---------------------------------------------------------------------------------------------------------------------
      
#==============================
# KNN (K-Nearest Neighbors)
#==============================
      
      # Loading the dataset
      trainKNN <- read.csv("data/train.csv")
      testKNN <- read.csv("data/test.csv")
      
      ## FEATURE ENGINEERING.
      
      # Rename columns of testing and training data set.
      setnames(trainKNN, names(trainKNN), c('date', 'category_predict', 'description_ignore', 'day_of_week', 'pd_district', 'resolution', 'address', 'x', 'y'))
      setnames(testKNN, names(testKNN), c('id', 'date', 'day_of_week', 'pd_district', 'address', 'x', 'y'))
      
      # Get hour of each crime.
      trainKNN$hour = as.numeric(substr(trainKNN$date, 12, 13))
      testKNN$hour = as.numeric(substr(testKNN$date, 12, 13))
      
      # Scaling dependent variables in 'train' to normalize the data.
      xTrainScale = scale(trainKNN$x)
      yTrainScale = scale(trainKNN$y)
      hourTrainScale = scale(trainKNN$hour)
      
      # Finalizing variables to be used for bulding the model
      trainModel = data.table(category_predict = trainKNN$category_predict, 
                               x_scaled = xTrainScale, 
                               y_scaled = yTrainScale, 
                               hour_scaled = hourTrainScale)
      setnames(trainModel, names(trainModel), 
               c('category_predict', 'x_scaled', 'y_scaled', 'hour_scaled'))
      
      # Create data subset to train NN on (training on full data set is too time-intensive).
      # 'subset' contains at least 1 example of every crime category.
      subsetKNN = trainModel[c(1:30000, 30024:33000, 33954:36000, 37298:38000, 41097:41900, 41980:43000, 44479:46000, 48707:48837, 49306:53715, 93717, 102637, 102645, 102678, 102919:103517, 103712:107734, 148476, 
                             192191:205046, 252094, 279792:300000, 316491, 317527:332821, 337881:350000)]
      dim(subsetKNN)
      
      # Set seed to ensure reproducibility.
      set.seed(125)
      
      # Create model and generate predictions for training set ('subset').
      knnTrain = kknn(formula = category_predict ~ x_scaled + y_scaled + hour_scaled, 
                       train = subsetKNN, 
                       test = subsetKNN, 
                       scale = T)
      
      
      # Generate predictions for training and test data.
      trainPred = data.table(knnTrain$fitted.values)
      
      #####
      # CHECK TRAINING SET ACCURACY.
      subsetKNN$pred = trainPred$V1
      
      # Calculating training set accuracy.
      print('Training Accuracy')
      print(table(subsetKNN$category_predict == subsetKNN$pred))
      print(prop.table(table(subsetKNN$category_predict == subsetKNN$pred)))
      
      #------------------------------------------------------
      # Results of Training Set
      #  FALSE      TRUE 
      # 0.4434623 0.5565377 
      #------------------------------------------------------
      
#============================================
# Regularized Logistic Regression (Dual)
#============================================
      
      # Loading the data
      train <- read.csv("data/train.csv")
      testSFOL2 <- read.csv("data/test.csv")
      
      # Splitting the timestamp values
      trainSFO_1 = split_date(train)
      testSFOL2 <- split_date(testSFOL2)
      
      # Binarizing categorical predictors in train set
      trainSFO_2 <- make_training_factors(trainSFO_1)
      target <- trainSFO_1$Category
      testSFOL2 = make_training_factors(testSFOL2)
      summary(testSFOL2)
      
      #Cleanup
      gc()
      
      # Removing NA/ NaN/ Inf values if present
      trainSFO_2 <- data.table(trainSFO_2)
      invisible(lapply(names(trainSFO_2),function(.name) set(trainSFO_2, which(is.infinite(trainSFO_2[[.name]])), j = .name,value =0)))
      trainSFO_2[is.na(trainSFO_2)] <- 0
      
      # Training a regularized logistic regression model on the training set
      logisticL2_Dual <- LiblineaR(trainSFO_2, target, type = 7, verbose = TRUE)
      
      # Applying the model on Test set and creating a submission file for Kaggle
      submit <- data.frame(predict(logisticL2_Dual,testSFOL2, proba = TRUE)$probabilities[, levels(target)])
      write.csv(submit, 'Ashok_Submit_L2.csv', row.names = F)

      # With this submission Kaggle gave a score of 2.55883 where highest score is 1.959.
      
#============================================
# Regularized Logistic Regression (Primal)
#============================================
      
      logisticL2_Primal <- LiblineaR(trainSFO_2, target, type = 0, verbose = FALSE)
      
      # Applying the model on Test set and creating a submission file for Kaggle
      submit1 <- data.frame(predict(logisticL2_Primal,testSFOL2, proba = TRUE)$probabilities[, levels(target)])
      write.csv(submit, 'Ashok_Submit_L2_Primal.csv', row.names = F)
      


#-----------------------------------------          
# LDA (Linear Discriminant Analysis)
#-----------------------------------------
      
      ctrl <- trainControl(method = "cv", classProbs = TRUE, number = 2)
      ldaFit <- train(x = trainSFO_2, 
                      y = make.names(response),
                      method = "lda",
                      preProc = c("center","scale"),
                      metric = "Accuracy",
                      trControl = ctrl)
      ldaFit
      
      ldaFit$pred <- merge(ldaFit$pred,  ldaFit$bestTune)
      ldaCM <- confusionMatrix(ldaFit, norm = "none")
      ldaCM
      
#---------------------------------------      
# GLMNET
#--------------------------------------
      
      glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), lambda = seq(.01, .2, length = 40))
      set.seed(476)
      glmnFit <- train(x = trainSFO_2, 
                       y = response,
                       method = "glmnet",
                       #tuneGrid = glmnGrid,
                       preProc = c("center", "scale"),
                       metric = "Accuracy")
      glmnFit
      
      glmnet2008 <- merge(glmnFit$pred,  glmnFit$bestTune)
      glmnetCM <- confusionMatrix(glmnFit, norm = "none")
      glmnetCM
      
#************************************************************************************************************
                                    # Classification Algorithms (Non-Linear)
#************************************************************************************************************
#============================================
# Extreme Gradient Boosting (XGB)
#============================================
      
      # Preparing the data
      trainMatrix <- as.matrix(sapply(trainSFO_2, as.numeric))
      testMatrix <- as.matrix(sapply(testSFOL2, as.numeric))
      
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
      
      
      bst.cv = xgb.cv(param=param, data = trainMatrix, label = trainSFO_1$Category, nfold = cv.nfold, nrounds = cv.nround)
      
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
      TestPredictionXGB <- data.frame(predict(bst,crimeSFOTest_2)$probabilities[, levels(target)])
      
      TestPredictionXGB$id = crimeSFOTest_1$id
      write.csv(TestPredictionXGB, 'test_pred_knn_numerical_only_benchmark.csv', row.names = F)
      
      
#------------------------------------------
# Naive Bayes
#------------------------------------------
      naiveBayes <- train(x = trainSFO_2, y = trainSFO_1$Category, 'nb')
      
      naivePred <- data.frame(predict(naiveBayes, test)$probabilities[, levels(target)])
      # Ran for almost two days without completing the execution.
