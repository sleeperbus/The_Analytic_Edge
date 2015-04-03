library(caTools)
library(ROCR)

stevens = read.csv("stevens.csv")
str(stevens)

split = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, split==T)
test = subset(stevens, split==F)
names(train)

# logistic model
mod.log = glm(Reverse ~ Circuit + Issue + Petitioner + Respondent +
                  LowerCourt + Unconst, data=train, family=binomial)
summary(mod.log)
pred.log = predict(mod.log, newdata=test, type="response")
prop.table(table(test$Reverse))
prop.table(table(test$Reverse, pred.log > 0.5))
logROCR = prediction(pred.log, test$Reverse)
auc = performance(logROCR, "auc")

