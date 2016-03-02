set.seed(1)
setwd("~/Faculdade/Data Mining/R scripts/IEEE Data Mining")

# 1 - Import the data:
train <- read.csv("sessao2_titanic_train.csv", na.strings = "", stringsAsFactors=TRUE)
test <- read.csv("sessao2_titanic_test.csv", na.strings = "", stringsAsFactors=TRUE)
test$Survived <- 0

# 2 - See missing values:
library(Amelia)
missmap(train)

# 2.1 Eliminate Cabin (much missing data), Ticket and Name for simplicity for now:
train <- train[,c(-4, -9, -11)]
test <- test[,c(-3, -8, -10)]

# 3 - Visualize the data:

table(train$Survived) # Check the number of survivors and dead
prop.table(table(train$Survived)) # Check the percentages

summary(train)
str(train)

# 4 - Generate a decision tree model:
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# 5 - Visualize the tree:
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

# 6 - Submit a solution (and then to kaggle):
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# Random forests:
# - Random forests models grow trees much deeper than rpart
#   - Use bagging => randomized sample of the training set (sample with replacement - repeated rows are allowed)
#   - Even using bagging, some variables could still dominate the first decisionin most of trees:
#       - Solution: use a selection of the available variables (square root)

# "Since each tree is grown out fully, they each overfit, but in different ways.
# Thus the mistakes one makes will be averaged out over them all."

# 7 - Prepare the data for random forest:
combi <- rbind(train, test)
missmap(combi)

# 7.1 - Eliminate NAs of variable Age
summary(combi$Age)

 # Train a model with the rows that have the age filled:
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Age)

# 7.2 - Eliminate the NAs of variable Embarked
summary(combi$Embarked)

combi$Embarked[is.na(combi$Embarked)] <- "S"
combi$Embarked <- factor(combi$Embarked)

# 7.3 - Eliminate the NA of variable Fare:
combi$Fare[is.na(combi$Fare)] <- median(combi$Fare, na.rm=TRUE)
summary(combi$Fare)
summary(combi)

train <- combi[1:891,]
test <- combi[892:1309,]

# 7 - Let's try a random forest:
library(randomForest)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, importance=TRUE, ntree=2000)

# Let's check what variables are more important:
varImpPlot(fit)

# 8 - Make another submission:
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)