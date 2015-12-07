stevens = read.csv("stevens.csv")
str(stevens)

# baseline 설정
table(stevens$Reverse)
baseline = 309/nrow(stevens)
# 무조건 reverse 라고 하면 반은 맞는다. 

# 데이터를 나누자.
library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, split == TRUE)
test = subset(stevens, split == FALSE)

form = Reverse ~ Term + Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst

# logistic regression 으로...
modLog = glm(form, data=train, family=binomial)
summary(modLog)
# AIC = 523.17, 중요하지 않은 변수를 줄여보자.
form2 = Reverse ~ Circuit + Respondent + LowerCourt
modLog2 = glm(form2, data=train, family=binomial)
summary(modLog2)
# AIC = 505.02 로 조금 줄었다. 그렇다면 이걸 사용해서 test set 을 검증하자.
predTestLog = predict(modLog2, newdata=test, type="response")
tblLog = table(test$Reverse, predTestLog > 0.5)
accuracyLog = (tblLog[1, 1] + tblLog[2, 2])/sum(tblLog)
# logistic model 의 정확도는 0.69 정도로 baseline 보다는 낫다. 

# 이제 CART 모델을 사용해보자. 
library(rpart)
library(rpart.plot)
formTree = Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst
modCART = rpart(formTree, data=train, method="class", minbucket=25)
summary(modCART)
prp(modCART)
# CART 를 사용해서 결과값을 예측해봅시다.
predTestCART = predict(modCART, newdata=test, type="class")
tblCART = table(test$Reverse, predTestCART)
accuracyCART = (tblCART[1, 1] + tblCART[2, 2])/sum(tblCART)
# higher than log model
library(ROCR)
predROC_CART = predict(modCART, newdata=test)
predictionCART = prediction(predROC_CART[, 2], test$Reverse)
performanceCART = performance(predictionCART, "tpr", "fpr")
plot(performanceCART, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(0.5, -0.5))
aucCART = as.numeric(performance(predictionCART, "auc")@y.values)

library(randomForest)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(train$Reverse)
modForest = randomForest(formTree, data=train, nodesize=25, ntree=200)
summary(modForest)
predTestForest = predict(modForest, newdata=test)
table(test$Reverse, predTestForest)
accuracyForest = (44+76)/(44+33+17+76)

library(caret)
library(e1071)
