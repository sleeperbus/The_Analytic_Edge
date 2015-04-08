claims = read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio=0.6)
train = subset(claims, split==T)
test = subset(claims, split==F)
train$bucket2008 = as.factor(train$bucket2008)
test$bucket2008 = as.factor(test$bucket2008)
train$bucket2009 = as.factor(train$bucket2009)
test$bucket2009 = as.factor(test$bucket2009)


# base line 
sum(diag(1, 5) * table(test$bucket2009, test$bucket2008))/nrow(test)
penaltyMatrix  = matrix(c(0, 1, 2, 3, 4, 2, 0 , 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 
                         0, 1, 8, 6, 4, 2, 0), nrow=5, ncol=5, byrow=T)
sum(as.matrix(table(test$bucket2009, test$bucket2008)) * penaltyMatrix)/nrow(test)

# formula 
library(rpart)
library(rpart.plot)
f = bucket2009 ~ age + alzheimers + arthritis + cancer + copd +
    depression + diabetes + heart.failure + ihd + kidney + 
    osteoporosis + stroke + reimbursement2008 + bucket2008

# Cross Validation
library(caret)
library(e1071)

numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.00001, 0.0002, 0.00001))
train(f, data=train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

# predict  
tree.mod = rpart(f, data=train, method="class", cp=0.00005)
prp(tree.mod)
tree.pred = predict(tree.mod, newdata=test, type="class")
sum(diag(1, 5) * table(test$bucket2009, tree.pred))/nrow(test)
sum(table(test$bucket2009, tree.pred) * penaltyMatrix)/nrow(test)

tree.mod2 = rpart(f, data=train, method="class", cp=0.00005, parms=list(loss=penaltyMatrix))
tree.pred2 = predict(tree.mod2, newdata=test, type="class")
sum(diag(1, 5) * table(test$bucket2009, tree.pred2))/nrow(test)
sum(table(test$bucket2009, tree.pred2) * penaltyMatrix)/nrow(test)
