# Install necesary packages:
#install.packages(c("ISLR", "Amelia", "ROCR"))

library(ISLR)
#attach(Default)
set.seed(1)

# Check if there are values missing
library(Amelia)

# Check more info
str(Default)
summary(Default)

missmap(Default)

# What does sample do?
#sample(4, 2)

# Build training set:
train.index <- sample(nrow(Default), 0.7 * nrow(Default))
train <- Default[train.index,]

# Build test set (why?):
test <- Default[-train.index,]
dim(train)
dim(test)

#### Build the model:
glm.fit <- glm(default ~ ., data=train, family=binomial)
summary(glm.fit)
glm.fit <- glm(default ~ . - income, data=train, family=binomial)
summary(glm.fit)

# What does contrasts do?
contrasts(Default$default)

#### Apply the test set:
glm.probs <- predict(glm.fit, test, type="response")
glm.pred <- ifelse(glm.probs > 0.5, 'Yes', 'No')

# Calculate overall accuracy:
confusion <- table(glm.pred, glm.test = test$default)

confusion

overallAccuracy <- function(table)
{
  (table[1,1] + table[2,2])/sum(table)
}

# TNR:
tnr <- function(table)
{
  table[1,1]/(table[1,1] + table[2,1])
}

# TPR:
tpr <- function(table)
{
  table[2,2]/(table[1,2] + table[2,2])
}

# Confirm the values:
#(2899 + 33)/3000
overallAccuracy(confusion)

#2899/(2899+13)
tnr(confusion)

#33/(33+55)
tpr(confusion)

# Number of 'Yes' in the train set:
sum(train$default == 'Yes')
sum(train$default == 'No')

# Calculate AUC:
library(ROCR)

calculateAuc <- function(predictedValues, trueValues)
{
  pr <- prediction(predictedValues, trueValues)
  prf <- performance(pr, measure = "tpr", x.measure="fpr")
  plot(prf)
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  auc
}

calculateAuc(predict(glm.fit, test), test$default)

## Change threshold:
glm.pred <- ifelse(glm.probs > .1, 'Yes', 'No')
confusion2 <- table(glm.pred, glm.test = test$default)

confusion2

# TNR:
#2713/(2713 + 199)
tnr(confusion2)

# TPR:
#63/(63+25)
tpr(confusion2)
