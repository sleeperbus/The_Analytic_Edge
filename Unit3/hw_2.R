parole = read.csv("parole.csv")
str(parole)
summary(parole)
prop.table(table(parole$violator))

parole$crime = as.factor(parole$crime)
parole$state = as.factor(parole$state)
summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == T)
test = subset(parole, split == F)

# model 1
mod1 = glm(violator ~ ., data=train, family=binomial)
summary(mod1)

t1 = data.frame(male=1, race=1, age=50, state=as.factor(1),
                time.served=3, max.sentence=12,
                multiple.offenses=0, crime=as.factor(2))
predict(mod1, newdata=t1)

# test set
pred1 = predict(mod1, newdata=test, type="response")
summary(pred1)
table(test$violator, pred1 > 0.5)
167/179
12/23
(167+12)/(167+12+11+12)
prop.table(table(train$violator))

library(ROCR)
ROCRpred = prediction(pred1, test$violator)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc = performance(ROCRpred, "auc")
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1))
