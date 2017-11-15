library("caret")
library("AppliedPredictiveModeling")
library("ElemStatLearn")
library("pgmm")
library("rpart")
library("randomForest")

set.seed(125)

# question 1
# Load the cell segmentation data from the AppliedPredictiveModeling package using the commands: 
data(segmentationOriginal)

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
# 3. In the final model what would be the final model prediction for cases with the following variable values:
# a. TotalIntenCh2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
# b. TotalIntenCh2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
# c. TotalIntenCh2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

train_id <- which(segmentationOriginal$Case == "Train")

dummy <- segmentationOriginal[, c(-1, -2)]

training <- dummy[train_id, ]
test <- dummy[-train_id, ]

model <- rpart(Class ~., data = training)
predict(model, newdata = test)

d1 <- test[0,]
d1[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)

d2 <- test[0,]
d2[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)

d3 <- test[0,]
d3[1,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)

d4 <- test[0,]
d4[1,c("FiberWidthCh1", "VarIntenCh4", "PerimStatusCh1")] <- c(8, 100, 2)

p1 <- predict(model, newdata = d1, type = "prob")
p2 <- predict(model, newdata = d2, type = "prob")
p3 <- predict(model, newdata = d3, type = "prob")
p4 <- predict(model, newdata = d4, type = "prob")

outs <- list(p1, p2, p3, p4)

sapply(outs, function(x) colnames(x)[which.max(x)])

# question 2
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample 
# (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of 
# out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out 
# cross validation? 

# question 3
# Load the olive oil data using the commands:
data(olive)
olive = olive[,-1]

# These data contain information on 572 different Italian olive oils from multiple regions in 
# Italy. Fit a classification tree where Area is the outcome variable. Then predict the value 
# of area for the following data frame using the tree command with all defaults

model <- rpart(Area ~ ., data = olive)

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata = newdata)

# What is the resulting prediction? Is the resulting prediction strange? Why or why not?

# question 4

# Load the South Africa Heart Disease Data and create training and test sets with the following code:

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size = dim(SAheart)[1]/2, replace = F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to 
# specify family="binomial") with CoronaryR Heart Disease (chd) as the outcome and age at onset, 
# current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low 
# density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your 
# model using this function and a prediction on the "response" scale:

set.seed(13234)

model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family = "binomial")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

# What is the misclassification rate on the training set? What is the misclassification 
# rate on the test set? 

missClass(trainSA$chd, predict(model, newdata = trainSA))
missClass(testSA$chd, predict(model, newdata = testSA))

# question 5
# Load the vowel.train and vowel.test data sets:

data(vowel.train)
data(vowel.test)

# Set the variable y to be a factor variable in both the training and test set. Then set the 
# seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining 
# variables. Read about variable importance in random forests here: 
# http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
# The caret package uses by default the Gini importance.

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

model <- randomForest(y ~., data = vowel.train)

# Calculate the variable importance using the varImp function in the caret package. 
# What is the order of variable importance?

out <- varImp(model)
row.names(out)[rev(order(out))]

# [NOTE: Use randomForest() specifically, not caret, as there's been some issues reported 
# with that approach. 11/6/2016]