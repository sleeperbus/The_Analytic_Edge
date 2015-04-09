hawk = read.csv("ClaimsData.csv")
str(hawk)

# split data
library(caTools)
set.seed(88)
split = sample.split(hawk$bucket2009, SplitRatio=0.6)
train = subset(hawk, split==T)
test = subset(hawk, split==F)
penaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), 
                       byrow=TRUE, nrow=5)
form = bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression +
  diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
  bucket2008 + reimbursement2008 

# base
base.table = table(test$bucket2009, test$bucket2008)
base.accu = sum(diag(1,5) * base.table)/nrow(test)
base.penalty= sum(base.table * penaltyMatrix)/nrow(test)

# cart
library(rpart)
library(rpart.plot)
cart.mod = rpart(form, data=train, method="class", cp=0.00005)
prp(cart.mod)
cart.pred = predict(cart.mod, newdata=test, type="class")
cart.table = table(test$bucket2009, cart.pred)
cart.accu = sum(diag(1, 5) * cart.table)/nrow(test)
cart.penalty = sum(penaltyMatrix * cart.table)/nrow(test)

# rebalance cart model
cart.mod2  = rpart(form, data=train, method="class", cp=0.00005, 
                   parms=list(loss=penaltyMatrix))
cart.pred2 = predict(cart.mod2, newdata=test, type="class")
cart.table2 = table(test$bucket2009, cart.pred2)
cart.accu2 = sum(diag(1, 5) * cart.table2)/nrow(test)
cart.penalty2 = sum(penaltyMatrix * cart.table2)/nrow(test)
