library(caTools)
library(corrplot)
library(ROCR)


quality = read.csv("quality.csv", stringsAsFactors = F)
str(quality)
prop.table(table(quality$PoorCare))
baseline = 98/nrow(quality)

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
train = subset(quality, split == T)
test = subset(quality, split == F)
prop.table(table(train$PoorCare))

# vars 간의 상관관계 분석
M = cor(quality)
corrplot(M)

# OfficeVisits, Narcotics 를 independent variable 로 봅시다.
logM = glm(PoorCare ~ OfficeVisits + Narcotics, data=train, family="binomial")
summary(logM)
# 결과값을 actual value 별로 모아서 값 평균을 봅시다.
tapply(logM$fitted.values, train$PoorCare, mean)

# 각 결과값의 범위를 좀 봅시다.
boxplot(logM$fitted.values ~ train$PoorCare)
# goodcare 인 것 중에서 fitted value 가 크게 나온 것들이 있다.
summary(logM$residuals)

# threshold 를 어떻게 정해야 할까?
table(train$PoorCare, logM$fitted.values > 0.5)
sensitivity = 10/25
specificity = 70/74

table(train$PoorCare, logM$fitted.values > 0.8)
sensitivity = 2/25
specificity = 73/74

table(train$PoorCare, logM$fitted.values > 1.3)
sensitivity = 13/25
specificity = 67/74

# ROCR 패키지를 사용해서 threshold 를 찾아봅시다.
ROCRpred = prediction(logM$fitted.values, train$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1))




# StartedOnCombination 을 포함한 모델 
logM_1 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data= train, family=binomial)
summary(logM_1)
