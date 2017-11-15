library("caret")
library("AppliedPredictiveModeling")
library("ElemStatLearn")
library("pgmm")
library("rpart")
library("randomForest")
library("gbm")
library("lubridate")
library("forecast")
library("e1071")

set.seed(125)

# question 1
# Load the vowel.train and vowel.test data sets:
data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. Then set the 
# seed to 33833. Fit (1) a random forest predictor relating the factor variable y to the 
# remaining variables and (2) a boosted predictor using the "gbm" method. Fit these both 
# with the train() command in the caret package.

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

m1 <- train(y ~., data = vowel.train, method = "rf")
m2 <- train(y ~., data = vowel.train, method = "gbm")

# What are the accuracies for the two approaches on the test data set? What is the 
# accuracy among the test set samples where the two methods agree? 

p1 <- predict(m1, newdata = vowel.test)
p2 <- predict(m2, newdata = vowel.test)

mean(p1 == vowel.test$y)
mean(p2 == vowel.test$y)
mean(p1 == p2)

# question 2
# Load the Alzheimer's data using the following commands
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Set the seed to 62433 and predict diagnosis with all the other variables using a 
# random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") 
# model. Stack the predictions together using random forests ("rf"). What is the resulting 
# accuracy on the test set? Is it better or worse than each of the individual predictions? 
set.seed(62433)

m1 <- train(diagnosis ~., data = training, method = "rf")
m2 <- train(diagnosis ~., data = training, method = "gbm")
m3 <- train(diagnosis ~., data = training, method = "lda")

preddf <- data.frame(p1 = predict(m1, newdata = testing),
                     p2 = predict(m2, newdata = testing),
                     p3 = predict(m3, newdata = testing),
                     diagnosis = testing$diagnosis)

mc <- train(diagnosis ~., data = preddf, method = "rf")

mean(testing$diagnosis == predict(m1, newdata = testing))
mean(testing$diagnosis == predict(m2, newdata = testing))
mean(testing$diagnosis == predict(m3, newdata = testing))
mean(preddf$diagnosis == predict(mc, newdata = preddf))

# question 3
# Load the concrete data with the commands:
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 233 and fit a lasso model to predict Compressive Strength. Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet). 

# question 4

# question 5
