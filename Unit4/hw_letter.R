letters = read.csv("letters_ABPR.csv")
str(letters)

letters$isB = as.factor(letters$letter == "B")
library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, split==T)
test = subset(letters, split==F)

# base line
prop.table(table(test$isB))


# CART model
library(rpart)
cart.mod = rpart(isB ~ . -letter, data=train, method="class")
cart.pred = predict(cart.mod, newdata=test, type="class")
cart.table = table(test$isB, cart.pred)
sum(diag(1, 2) * cart.table) / sum(cart.table)

# random forest
library(randomForest)
forest.mod = randomForest(isB ~ . -letter, data=train)
forest.pred = predict(forest.mod, newdata=test)
forest.table = table(test$isB, forest.pred)
sum(diag(1, 2) * forest.table)/sum(forest.table)

# New model
set.seed(2000)
split = sample.split(letters$letter, SplitRatio=0.5)
train = subset(letters, split==T)
test = subset(letters, split==F)

# base line
prop.table(table(train$letter))
prop.table(table(test$letter))

# CART model
cart.mod = rpart(letter ~ . -isB, data=train, method="class")
cart.pred = predict(cart.mod, newdata=test, type="class")
cart.table = table(test$letter, cart.pred)
sum(diag(1,4) * cart.table)/sum(cart.table)

# random forest
set.seed(1000)
forest.mod = randomForest(letter ~ . -isB, data=train)
forest.pred = predict(forest.mod, newdata=test)
forest.table = table(test$letter, forest.pred)
sum(diag(1,4)*forest.table)/sum(forest.table)
