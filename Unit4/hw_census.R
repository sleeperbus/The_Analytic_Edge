census = read.csv("census.csv")
str(census)

library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, split==T)
test = subset(census, split==F)

# base line 
prop.table(table(train$over50k))
prop.table(table(test$over50k))

# logistic model
log.mod = glm(over50k ~ ., data=train, family=binomial)
summary(log.mod)
log.pred = predict(log.mod, newdata=test, type="response")
log.table = table(test$over50k, log.pred > 0.5)
sum(diag(1,2) * log.table)/sum(log.table)
library(ROCR)
log.prediction = prediction(log.pred, test$over50k)
log.auc = performance(log.prediction, "auc")
log.auc
log.perf = performance(log.prediction, "tpr", "fpr")
plot(log.perf, colorize=T, print.cutoffs.at=seq(0,1,0.10), text.adj=c(0,1.7))


# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(over50k ~ ., data=train, method="class")
prp(cart.mod)
cart.pred = predict(cart.mod, newdata=test, type="class")
cart.table = table(test$over50k, cart.pred)
sum(diag(1,2)*cart.table)/sum(cart.table)

cart.pred = predict(cart.mod, newdata=test)
cart.prediction = prediction(cart.pred[,2], test$over50k)
cart.perf = performance(cart.prediction, "tpr", "fpr")
plot(cart.perf, print.cutoffs.at=seq(0,1,0.1), colorize=T, text.adj=c(0,1.7))
