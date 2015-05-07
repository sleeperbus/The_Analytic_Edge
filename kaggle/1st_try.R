library(caTools)
library(ROCR)

news = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=F)
str(news)

# exploratory data
table(news$Popular, news$NewsDesk)
summary(news$WordCount)
boxplot(news$WordCount)
hist(news$WordCount, breaks=50)
rug(news$WordCount)

# split data
spl = sample.split(news$Popular, SplitRatio=0.7)
train = subset(news, spl==T)
test = subset(news, spl==F)

# model - just use WordCount
log.mod = glm(Popular ~ WordCount, data=train, family=binomial)
log.pred.train= predict(log.mod, type="response")
log.prediction.train= prediction(log.pred.train, train$Popular)
log.performance.train= performance(log.prediction.train, "tpr", "fpr")
plot(log.performance.train, colorize=T, print.cutoffs.at=seq(0,1,0.05),
     text.adj=c(0,1.7))
log.table.train = table(train$Popular, log.pred.train > 0.15)
sum(diag(1,2)*log.table.train)/sum(log.table.train)
log.auc.train = performance(log.prediction.train, "auc")@y.values

log.pred.test = predict(log.mod, newdata=test, type="response")
log.prediction.test = prediction(log.pred.test, test$Popular)
log.performance.test = performance(log.prediction.test, "tpr", "fpr")
plot(log.performance.test, colorize=T, print.cutoffs.at=seq(0,1,0.05),
     text.adj=c(0,1.7))
log.table.test= table(test$Popular, log.pred.test > 0.15)
sum(diag(1,2)*log.table.test)/sum(log.table.test)
log.auc.test = performance(log.prediction.test, "auc")@y.values
