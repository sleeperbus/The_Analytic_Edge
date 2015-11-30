framingham = read.csv("framingham.csv")
str(framingham)
summary(framingham)
hist(framingham$age)
table(framingham$TenYearCHD)
baseline = 3596/nrow(framingham)
# 앞으로 만들 모델은 최소한 0.8481 보다는 커야한다.

library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

# 모든 var 를 사용해봅시다.
logM_all = glm(TenYearCHD ~ ., data=train, family=binomial)
summary(logM_all)
nrow(train)
length(logM_all$fitted.values)
# 길이가 다르다.
    

# TenYearCHD 에 따른 평균값들을 봅시다. NA 를 방지하기 위해 newdata 로 사용
predictTrain = predict(logM_all, type="response", newdata=train)
tapply(predictTrain, train$TenYearCHD, mean, na.rm=TRUE)
# 애매하네... 발병률을 예측하는 값이 거의 차이가 안 나네.
# 그렇다면 ROCR 을 봅시다.
library(ROCR)
ROCRpredTrain = prediction(predictTrain, train$TenYearCHD)
ROCRperfTrain = performance(ROCRpredTrain, "tpr", "fpr")
plot(ROCRperfTrain, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-1, 0.3))
# threshold 는 0.2 정도가 적당한 듯
table(train$TenYearCHD, predictTrain > 0.2)
accuracy = (1603 + 186) / (1603 + 423 + 173 + 186)
# 베이스라인보다 훨씬 못하다... 
# 결과의 분포를 좀 봅시다. 상당한 부분이 겹쳐져 있네. 어느 threshold 를 선택하던
# 오류가 상당함
boxplot(predictTrain ~ train$TenYearCHD)
# 그럼 auc 라도 봅시다.  
auc = as.numeric(performance(ROCRpredTrain, "auc")@y.values)
# 0.735 정도...
# 만약 threshold 를 0.5로 잡으면?
table(train$TenYearCHD, predictTrain > 0.5)
accuracy = (2019 + 25) / (2019 + 7 + 334 + 25)
# 정확도는 0.85 정도로 baseline 가 비슷하게 나온다. 그런데 문제는... TN 는 
# 거의 다 맞추는데 TP 는 거의 다 놓치네... 강의에서는 저위험군을 고위험군과
# 잘 분리한다고 설명하기는 했는데, 고위험군을 더 빨리 찾아야 하는 것 아닌가?

# 어쨌든 test set 에 적용해봅시다.
predTest = predict(logM_all, newdata=test, type="response")
table(test$TenYearCHD, predTest > 0.5)

