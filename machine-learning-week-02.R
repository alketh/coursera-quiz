library("caret")
library("AppliedPredictiveModeling")
library("Hmisc")

# question 1
# Load the Alzheimer's disease data using the commands:
data(AlzheimerDisease)

# Which of the following commands will create non-overlapping training and test sets with about 
# 50% of the observations assigned to each? 

adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# question 2
# Load the cement data using the commands:
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

# Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color 
# by each of the variables in the data set (you may find the cut2() function in the Hmisc 
# package useful for turning continuous covariates into factors). What do you notice in these plots?

training$index <- 1:nrow(training)

for (i in 1:(ncol(training) - 2)) {
  training[, i] <- cut2(training[, i], g = 5)
}

ggplot(training, aes(x = index, y = CompressiveStrength, color = Cement)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = BlastFurnaceSlag)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = FlyAsh)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = Water)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = Superplasticizer)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = CoarseAggregate)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = FineAggregate)) +
  geom_point()

ggplot(training, aes(x = index, y = CompressiveStrength, color = Age)) +
  geom_point()

# question 3
# Load the cement data using the commands:

data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer)
hist(log(training$Superplasticizer))
plot(training$Superplasticizer)
min(training$Superplasticizer)

# question 4
# Load the Alzheimer's disease data using the commands:

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)

col_ids <- which(grepl(pattern = "^IL", names(training)))
train_pca <- training[, c(1, col_ids)]

wuwu <- preProcess(train_pca, method = "pca", thresh = 0.9)
wuwu$numComp

# question 5
# Load the Alzheimer's disease data using the commands:

set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,] 
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors with variable names beginning
# with IL and the diagnosis. Build two predictive models, one using the predictors as they are 
# and one using PCA with principal components explaining 80% of the variance in the predictors.
# Use method="glm" in the train function.
# What is the accuracy of each method in the test set? Which is more accurate?

col_ids <- which(grepl(pattern = "^IL", names(training)))
training <- training[, c(1, col_ids)]

# NON-PCA + glm
p1 <- train(diagnosis ~ ., data = training, method = "glm")
pred1 <- predict(p1, newdata = testing)
confusionMatrix(pred1, testing$diagnosis)$overall[1]

# APPLY PCA + glm
train_pre <- preProcess(training[, -1], method = "pca", thresh = 0.8)
train_pc <- predict(train_pre, training[, -1])
train_pc$diagnosis <- training$diagnosis
p2 <- train(diagnosis ~., method = "glm", data = train_pc)
pred2 <- predict(train_pre, newdata = testing)
confusionMatrix(testing$diagnosis, predict(p2, pred2))$overall[1]
