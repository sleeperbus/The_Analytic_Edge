set.seed(144)
loans = read.csv("loans_imputed.csv")
str(loans)
summary(loans)

split = sample.split(loans$not.fully.paid, SplitRatio=0.7)
train= subset(loans, split == T)
test = subset(loans, split == F)

# model 1
mod1 = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(mod1)

# test set
test$predicted.risk = predict(mod1, newdata=test, type="response")
prop.table(table(test$not.fully.paid, test$predicted.risk > 0.5))
prop.table(table(test$not.fully.paid))

library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
auc= performance(ROCRpred, "auc")
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(0, 1.7))

# model 2
mod2 = glm(not.fully.paid ~ int.rate, data=train, family=binomial)
summary(mod2)
pred.test = predict(mod2, newdata=test, type="response")
summary(pred.test)
table(test$not.fully.paid, pred.test > 0.5)
auc2 = performance(prediction(pred.test, test$not.fully.paid), "auc")

# interest
test$profit = (exp(test$int.rate * 3) - 1)
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)

highinterest = subset(test, int.rate > 0.15)
summary(highinterest)
prop.table(table(highinterest$not.fully.paid))
cutoff = sort(highinterest$predicted.risk, decreasing = F)[100]
selectedLoans = subset(highinterest, predicted.risk <= cutoff)
nrow(selectedLoans)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
