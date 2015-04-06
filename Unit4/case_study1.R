library(caTools)
library(ROCR)
library(caret)

set.seed(3000)
stevens = read.csv("stevens.csv")
str(stevens)
split = sample.split(stevens$Reverse, SplitRatio=0.7) 
train = subset(stevens, split==T)
test = subset(stevens, split==F)
names(train)

form = Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt +
        Unconst
# logistic model
log.mod = glm(form, data=train, family=binomial)
summary(log.mod)
log.pred = predict(log.mod, newdata=test, type="response")
prop.table(table(test$Reverse))
prop.table(table(test$Reverse, log.pred > 0.5))
log.ROCR = prediction(log.pred, test$Reverse)  
log.auc = performance(log.ROCR, "auc")
log.perf = performance(log.ROCR, "tpr", "fpr")
plot(log.perf, colorize=T, print.cutoffs.at=seq(0.1, 1, 0.1), 
     text.adj=c(0, 1.7))

# CART model
library(rpart)
library(rpart.plot)
library(randomForest)

tree.mod = rpart(form, data=train, method="class", minbucket=25)
summary(tree.mod)
prp(tree.mod)

# CART Model prediction
tree.pred = predict(tree.mod, newdata=test, type="class")
prop.table(table(test$Reverse, tree.pred))

# CART ROC Curve
tree.roc.prob= predict(tree.mod, newdata=test)
tree.roc.pred = prediction(tree.roc.prob[,2], test$Reverse)
tree.roc.auc = performance(tree.roc.pred, "auc")
tree.roc.perf = performance(tree.roc.pred, "tpr", "fpr")
plot(tree.roc.perf, print.cutoffs.at=seq(0.1, 1, 0.1), colorize=T, 
     text.adj=c(0, 1.7))


# Random Forest Model
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)
forest.mod = randomForest(form, data=train, ntree=200, nodesize=25)
forest.pred = predict(forest.mod, newdata=test)
prop.table(table(test$Reverse, forest.pred))
           
