## Data Set

#Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## The goal of this project is to predict the manner in which they did the exercise. This is the "class" variable in the training set.

### Load caret, rpart, rpart.plot, RColorBrewer, rattle, randomForest packages

library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)


### Set seed to reproduce same results 

set.seed(12345)

### Loading data 
train <- "data/pml-training.csv"
test <- "data/pml-testing.csv"
training <- read.csv(train, na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(test, na.strings=c("NA","#DIV/0!",""))


### Partitioning the dataset into training and testing 

#60% data is considered for the training set and remaining 40% is considered for testing the model.

#A line of code is written to view the dimensions of the dataset.

inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
dim(myTraining)
dim(myTesting)


### Data Cleansing

#Below transformations have been used to clean the dataset.


myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)
myNZVvars <- names(myTraining) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
"kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
"amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
"stddev_yaw_forearm", "var_yaw_forearm")
myTraining <- myTraining[!myNZVvars]
# Review the revised value of observations
dim(myTraining)


### Remove first column of the dataset (ID) to ensure ML Algorithm can run smoothly
myTraining <- myTraining[c(-1)]


### Updating the variables having NA values and setting back to original dataset

trainingV3 <- myTraining 
for(i in 1:length(myTraining)) { 
if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .6 ) { 
for(j in 1:length(trainingV3)) {
if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { 
trainingV3 <- trainingV3[ , -j] #Remove that column
}
}
}
}
dim(trainingV3)
myTraining <- trainingV3
rm(trainingV3)

### Reproduce same transformations on the testing dataset and view dimensions of the testing dataset

clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58])
myTesting <- myTesting[clean1]
testing <- testing[clean2]
dim(myTesting)
dim(testing)

### In order to ensure proper functioning of Decision Trees and especially RandomForest Algorithm with the Test data set, we need to coerce the data into the same type.

for (i in 1:length(testing) ) {
for(j in 1:length(myTraining)) {
if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
class(testing[j]) <- class(myTraining[i])
}
}
}
testing <- rbind(myTraining[2, -58] , testing)
testing <- testing[-1,]


### Development of ML Algortihms for Predictions: Decision Tree


modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")


### Develop Decision Tree


fancyRpartPlot(modFitA1)


### Prediction Algorithm and Confusion Matrix

predictionsA1 <- predict(modFitA1, myTesting, type = "class")
confusionMatrix(predictionsA1, myTesting$classe)

### Development of ML Algortihms for Predictions: Random Forest

modFitB1 <- randomForest(classe ~. , data=myTraining)
predictionsB1 <- predict(modFitB1, myTesting, type = "class")
confusionMatrix(predictionsB1, myTesting$classe)


## Creating files for executing the test cases

predictionsB2 <- predict(modFitB1, testing, type = "class")

### Code to automatically create the files and copy in your current working directory
#Please change the directory using setwd() command in case if you want to change the working directory where files should be saved.

pml_write_files = function(x){
n = length(x)
for(i in 1:n){
filename = paste0("problem_id_",i,".txt")
write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}
pml_write_files(predictionsB2)
