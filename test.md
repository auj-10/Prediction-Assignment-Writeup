---
title: "Prediction Assignment Writeup"
author: "Akshay Jagtap"
date: "06/08/2020"
output: html_document
---
```{css, echo=FALSE}
p {
  text-align: justify;
}
```
### Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).
<br>

### Data
The training data for this project are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 
<br>

### Task
The goal is to predict the manner in which they did the exercise. This is the **`classe`** variable in the training set.

1. Create a report describing how you built your model.
2. How you used cross validation.
3. What you think the expected out of sample error is.
4. Use your prediction model to predict 20 different test cases.
<br>

### Loading the data

```{r}
trainingData = read.csv('Practical Machine Learning/pml-training.csv', na.strings = c("NA","#DIV/0!",""))
testingData = read.csv('Practical Machine Learning/pml-testing.csv', na.strings = c("NA","#DIV/0!",""))
```

### Data cleaning
1. Removing the data having NA values, with a threshold of 95%

```{r}
naIndex = (colSums(is.na(trainingData)) / nrow(trainingData)) < 0.95
training = trainingData[, naIndex]
testing = testingData[, naIndex]
dim(training)
dim(testing)
```
<br>

2. Removing the data having near zero variance

```{r}
library(caret)
zeroVar = nearZeroVar(training)
training = training[, -zeroVar]
testing = testing[, -zeroVar]
dim(training)
dim(testing)
```
<br>
    
3. Removing non-numerical data

```{r}
names(training)
training = training[, -c(1:6)]
testing = testing[, -c(1:6)]
dim(training)
dim(testing)
```

### Data partitioning

```{r}
set.seed(3433)
inTrain = createDataPartition(training$classe, p = 0.6)[[1]]
training_train = training[inTrain,]
training_cross_val = training[-inTrain,]
```

### Model Fitting
1. **Decision tree**

```{r fig.align="center"}
dtFit = train(classe~., data = training_train, method="rpart", trControl = trainControl(method = "cv", number = 3))
library(rpart.plot)
rpart.plot(dtFit$finalModel)
dtPred = predict(dtFit, training_cross_val)
confusionMatrix(dtPred, training_cross_val$classe)
```
Accuracy: ~49%
<br><br>

2. **Gradient Boosting Model**

```{r}
gbmFit = train(classe~., data = training_train, method="gbm", trControl = trainControl(method = "cv", number = 3), verbose = FALSE)
gbmPred = predict(gbmFit, training_cross_val)
confusionMatrix(gbmPred, training_cross_val$classe)
```
Accuracy: ~96%
<br><br>

3. **Random forest**

```{r}
library(randomForest)
rfFit = randomForest(classe~., data = training_train, ntree = 1000)
rfPred = predict(rfFit, training_cross_val)
confusionMatrix(rfPred, training_cross_val$classe)
```
Accuracy: ~99%

### Out of sample error
Considering the random forest model, the out of sample error is ~0.5% which implies the model should predict the class correctly.

### Prediction
The models used are: Decision Tree, GBM and Random Forest. Among the models, the random forest has the highest accuracy. It is chosen for prediction.

```{r}
pred = predict(rfFit, testing)
pred
```
