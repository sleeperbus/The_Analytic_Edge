stocks = read.csv("StocksCluster.csv")
str(stocks)
table(stocks$PositiveDec)
6324/nrow(stocks)
cor(stocks)
summary(stocks)

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio=0.7)
stockTrain = subset(stocks, spl==T)
stockTest = subset(stocks, spl==F)
modLog= glm(PositiveDec ~ ., data=stockTrain, family=binomial)
summary(modLog)
predTrainLog= predict(modLog, type="response")
tableTrainLog= table(stockTrain$PositiveDec, predTrainLog> 0.5)
sum(diag(1,2)*tableTrainLog)/sum(tableTrainLog)

predTestLog = predict(modLog, newdata=stockTest, type="response")
tableTestLog = table(stockTest$PositiveDec, predTestLog > 0.5)
sum(diag(1,2)*tableTestLog)/sum(tableTestLog)

# base line
table(stockTest$PositiveDec)
1897/nrow(stockTest)

limitedTrain = stockTrain
limitedTrain$PositiveDec = NULL
limitedTest = stockTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest) 
summary(normTrain)

set.seed(144)
km = kmeans(normTrain, centers=3)
table(km$cluster)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stockTrain, clusterTrain == 1)
stocksTrain2 = subset(stockTrain, clusterTrain == 2)
stocksTrain3 = subset(stockTrain, clusterTrain == 3)
stocksTest1 = subset(stockTest, clusterTest == 1)
stocksTest2 = subset(stockTest, clusterTest == 2)
stocksTest3 = subset(stockTest, clusterTest == 3)

stocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
stocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
stocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)

summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)

predictTest1 = predict(stocksModel1, newdata=stocksTest1, type="response")
predictTest2 = predict(stocksModel2, newdata=stocksTest2, type="response")
predictTest3 = predict(stocksModel3, newdata=stocksTest3, type="response")

table1 = table(stocksTest1$PositiveDec, predictTest1 > 0.5)
table2 = table(stocksTest2$PositiveDec, predictTest2 > 0.5)
table3 = table(stocksTest3$PositiveDec, predictTest3 > 0.5)
sum(diag(1,2)*table1)/sum(table1)
sum(diag(1,2)*table2)/sum(table2)
sum(diag(1,2)*table3)/sum(table3)

allPredictions = c(predictTest1, predictTest2, predictTest3)
allOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
allTable = table(allOutcomes, allPredictions > 0.5)
sum(diag(1,2)*allTable)/sum(allTable)
