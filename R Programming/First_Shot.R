# install.packages("glmnet")
library(ggmap)
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


# Reading the dataset
crimeSFO <- read.csv("train.csv")

## 75% of the sample size
samp_size <- floor(0.75 * nrow(crimeSFO))

## set the seed to make your partition reproductible
set.seed(456)
train_index <- sample(seq_len(nrow(crimeSFO)), size = samp_size)

trainSFO <- crimeSFO[train_index, ]
testSFO <- crimeSFO[-train_index, ]

map <- get_map(location="sanfrancisco",zoom=12,source="osm")
plot(map)


# functions to make variables from date info.

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

# Creating all date and related variables
t <- Sys.time(); trainSFO_1 = split_date(trainSFO); Sys.time()-t

t <- Sys.time(); testSFO_1 = split_date(testSFO); Sys.time()-t

# Types of crimes and its count
data_plot1 = trainSFO_1 %>%
      group_by(Category) %>%
      summarise(count = n()) %>%
      transform(Category = reorder(Category,-count))

ggplot(data_plot1) + 
      geom_bar(aes(x=Category, y=count, 
                   color = Category, fill = Category),
               stat="identity")+
      coord_flip()+
      theme(legend.position="None")+
      ggtitle("Number of crimes in individual category")+
      xlab("Category of crime")+
      ylab("Number of crimes")


data_plot1 = data_plot1[with(data_plot1,order(-count)),]
Top_crimes = data_plot1

print("Top 10 crimes")
top_10 = Top_crimes[1:10,1]


head(Top_crimes,20)
df = data.frame()
sum = 0
for (i in 1:dim(Top_crimes)[1]){
      sum = sum + Top_crimes[i,2] 
      Top_crimes$CumCr[i] = sum/sum(Top_crimes$count)
}

per_20 = Top_crimes$CumCr[20]*100
print(paste("Percentage of crimes in top 20 categories = " ,
            as.character(per_20)))


## Crime category by week, month and hour
data_plot2 = trainSFO_1 %>%
      subset(Category %in% top_10) %>%
      group_by(Years,Category) %>%
      summarise(count = n()) 

data_plot2$Category = factor(data_plot2$Category,levels = top_10)

ggplot(data = data_plot2,aes(x=Years, y=count,fill = Category)) + 
      geom_boxplot() + 
      facet_wrap(~Category,ncol = 5)+
      theme(legend.position="None",
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Year")+
      ylab("Number of crime incidents")+
      ggtitle("Variations in crime by year")+
      xlab("Number of crimes")+
      ylab("Year")

# Crime vs DayOfWeek

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


# Crime vs hour of the day
data_plot = trainSFO_1 %>%
      group_by(Hour,Years,Month,YearsMo) %>%
      summarise(count = n()) 
p3 = ggplot(data = data_plot,aes(x=Hour, y=count,fill = Hour)) + 
      geom_boxplot() + 
      xlab("Hour of day")+
      ylab("Number of crime incidents")+
      theme(legend.position="None")


grid.arrange(p1,p2,p3,ncol = 1,top="Important factors affecting crime rates")



# Google Vis
crime_Density <- as.data.frame(table(trainSFO$Category))

data_plot2 <- as.data.frame(data_plot2)
write.csv(data_plot2, file = 'crime_year.csv')

## Visualzation in motion charts using "googleVis" package

# Visualizing Crime Density over the years

crime_Density <- read.csv("crime_year.csv", header = TRUE)
head(crime_Density)
crime_den = gvisMotionChart(crime_Density, 
                      idvar="Category", 
                      timevar="Years",
                      options = list(width=1300, height=800,  title='Crime Density Over the Years'))

plot(crime_den)

# Splitting the categorical predictors in training data
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

trainSFO_2 <- make_training_factors(trainSFO_1)

response <- trainSFO_1$Category

# Linear Model
firstLinear <- LiblineaR(trainSFO_2, response, type = 1, verbose = FALSE)

# Predicting on Test set
testSFO_2 <- make_training_factors(testSFO_1)

testPred <- predict(firstLinear, testSFO_2)

predOutput <- as.data.frame(testSFO_1$Category)
predOutput$Observed <- testPred$predictions
colnames(predOutput) <- c('obs', 'pred')

defaultSummary(predOutput)

lmPred <- lm(trainSFO_2, response)

# # First Classification Model
# trainSFO_1 <- trainSFO_1[,-c(1,3,7,14)]
# 
# library(kernlab)
# set.seed(1056)
# svmFit <- train(Category ~ ., data = trainSFO_1, method = "svmRadial", preProc = c("center", "scale"), tuneLength = 10,trControl = trainControl(method = "repeatedcv", repeats = 5))
# svmFit
# 

# Principle Component Analysis
solPCA <- prcomp(trainSFO_2, center = TRUE, scale. = TRUE)
percentVariancePCA = solPCA$sd^2/sum(solPCA$sd^2)*100
plot(percentVariancePCA, xlab="Components", ylab="Percentage of Total Variance", type="l", main="Scree Plot of PCA Analysis")

#-----------Linear Models---------------

# LDA
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

# GLMNET
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

# # Logistic Regression
# set.seed(1056)
t <- Sys.time(); 
logisticReg <- train(x = trainSFO_2, y = response, method = "glm"); Sys.time()-t

#---------------Non-Linear Models----------------

# Neural Networks
nnetGrid <- expand.grid(size = c(1,2), decay = c(0, 1))
maxSize <- max(nnetGrid$size)

## Four different models are evaluate based on the data pre-processing and 
## whether a single or multiple models are used

set.seed(476)
t <- Sys.time(); 
nnetFit <- train(x = trainSFO_2, 
                 y = response,
                 method = "nnet",
                 metric = "Accuracy",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(trainSFO_2) + 1) + maxSize + 1)); Sys.time()-t
nnetFit
max(nnetFit$results$Accuracy, na.rm = TRUE)


# Naive Bayes
t <- Sys.time(); naiveFit <- train(x = trainSFO_2, y = response, method = "nb"); Sys.time()-t
naiveFit

### K-Nearest Neighbors

set.seed(476)
knnFit <- train(x = trainSFO_2, 
                y = response,
                method = "knn",
                metric = "Accuracy",
                preProc = c("center", "scale"))
knnFit
max(knnFit$results$Accuracy)

knnFit$pred <- merge(knnFit$pred,  knnFit$bestTune)
knnCM <- confusionMatrix(knnFit, norm = "none")
knnCM

update(plot(knnFit, ylab = "ROC (2008 Hold-Out Data)"))

### Section 13.4 Support Vector Machines

library(kernlab)

set.seed(201)
sigmaRange <- sigest(as.matrix(trainSFO_2))
svmRGrid <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
                        C = 2^(-1:1))
set.seed(476)
svmRFit <- train(x = trainSFO_2, 
                 y = response,
                 method = "svmRadial",
                 metric = "Accuracy",
                 preProc = c("center", "scale"),
                 tuneGrid = svmRGrid)
svmRFit
max(svmRFit$results$Accuracy)

# MDA
mdaFit <- train(x = trainSFO_2, 
                y = response,
                method = "mda",
                metric = "Accuracy",
                tries = 40,
                tuneGrid = expand.grid(subclasses = 1:8))
mdaFit
max(mdaFit$results$Accuracy)

mdaFit$pred <- merge(mdaFit$pred,  mdaFit$bestTune)
mdaCM <- confusionMatrix(mdaFit, norm = "none")
mdaCM



## Test KNN

train <- read.csv("train.csv")
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

# Create 'train_model' and 'test_model' which only include variables used in the model.
train_model = data.table(category_predict = train$category_predict, 
                         x_scaled = x_train_scaled, 
                         y_scaled = y_train_scaled, 
                         hour_scaled = hour_train_scaled)
setnames(train_model, 
         names(train_model), 
         c('category_predict', 'x_scaled', 'y_scaled', 'hour_scaled'))

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

# Generate predictions for training and test data.
# Training predictions are computed for 'subset', not 'train' (due to time constraints).
# For the training data ('subset'), I only want to compute accuracy.
# For the test data, I need to put predictions in a specific format for submission, as specified by Kaggle.com.
# Training and test predictions  were already computed when generating model.
train_pred = data.table(knn_train$fitted.values)

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
