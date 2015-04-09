court = read.csv("stevens.csv")
str(court)
court$Unconst = as.factor(court$Unconst)
court$Reverse = as.factor(court$Reverse)

form = Reverse ~ Circuit + Issue + Petitioner + Respondent + 
    LowerCourt + Unconst 
library(caTools)
set.seed(3000) 
split = sample.split(court$Reverse, SplitRatio=0.7)
train = subset(court, split==T)
test = subset(court, split==F)

# baseline model 
prop.table(table(test$Reverse))


# logistic regression model 
log.mod = glm(form, data=train, family=binomial)
summary(log.mod)
log.pred = predict(log.mod, newdata=test, type="response")
summary(log.pred)
library(ROCR)
log.rocr.pred = prediction(log.pred, test$Reverse)
log.rocr.perf = performance(log.rocr.pred, "tpr", "fpr")
plot(log.rocr.perf, colorize=T, print.cutoffs.at=seq(0,1,0.1),
     text.adj=c(0, 1.7))
table(test$Reverse, log.pred > 0.6)
(56+56)/(56+21+37+56)

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(form, data=train, method="class", minbucket=25)
prp(cart.mod)
plot(cart.mod)
text(cart.mod)
cart.pred = predict(cart.mod, newdata=test, type="class")
table(test$Reverse, cart.pred)
(41+71)/(41+36+22+71)
cart.rocr.pred = prediction(cart.pred, test$Reverse)

cart.pred2 = predict(cart.mod, newdata=test)
cart.rocr.pred = prediction(cart.pred2[, 2], test$Reverse)
cart.rocr.perf = performance(cart.rocr.pred, "tpr", "fpr")
plot(cart.rocr.perf, print.cutoffs.at=seq(0,1,0.1), text=c(0,1.7),
     colorize=T)
  
# random forest
library(randomForest)
forest.mod = randomForest(form, data=train, nodesize=25, ntree=200)
forest.pred = predict(forest.mod, newdata=test)
table(test$Reverse, forest.pred)
(42+72)/(42+35+21+72)

# cross validation
library(caret)
library(e1071)
trCont = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(form, data=train, method="rpart", trControl = trCont, tuneGrid = cpGrid)

cv.mod = rpart(form, data=train, method="class", cp=0.19)
cv.pred = predict(cv.mod, newdata=test, type="class")
table(test$Reverse, cv.pred)
(59+64)/(59+18+29+64)
