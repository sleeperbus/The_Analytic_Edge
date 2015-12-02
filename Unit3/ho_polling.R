poll = read.csv("PollingData.csv")
str(poll)
summary(poll)
# Rasmussen 과 SurveyUSA 에 누락된 자료가 많다. 이 자료들 채워넣어보자.

library(mice)
imputed = complete(mice(poll[, c("Rasmussen", "SurveyUSA", "DiffCount", "PropR")]))
poll$Rasmussen = imputed$Rasmussen
poll$SurveyUSA = imputed$SurveyUSA

# 각 변수간의 상관관계를 봅시다.
library(corrplot)
dfCor = poll[, c("Rasmussen", "SurveyUSA", "DiffCount", "PropR", "Republican")]
M = cor(dfCor)
corrplot.mixed(M)

train = subset(poll, Year < 2012)
test = subset(poll, Year == 2012)

# train set 의 baseline 은?
table(train$Republican)
baseline = 53/nrow(train)
# 이렇게 하면 오답률이 너무 높다.
table(train$Republican, sign(train$Rasmussen))
baseline = (42+52)/nrow(train)
# Rasmussen 의 설문조사로만 따라도 0.94 정도의 적중률이 나온다. 그렇다면 
# 이것보다는 좋은 적중률을 보여야 한다.
# 그런데 Rasmussen 이 0인 것들은 무엇들인가?
subset(train, Rasmussen == 0)


# 모든 변수를 다 사용하는 모델을 만들어 봅시다.
mod_1 = glm(Republican ~ Rasmussen + SurveyUSA + DiffCount + PropR, data=train, family=binomial)
summary(mod_1)
# p-value 가 중요한 것들이 없다. multicollinearty 때문인가.
predMod_1 = predict(mod_1, type="response")
table(train$Republican, predMod_1 > 0.5)
accuracy_1 = (45+52)/(45+52+2+1)

# 그렇다면 상관관계가 가장 높은 PropR 만 우선 사용해보자.
mod_2 = glm(Republican ~ PropR, data=train, family=binomial)
summary(mod_2)
predMod_2 = predict(mod_2, type="response")
table(train$Republican, predMod_2 > 0.5)
accuracy_2 = (45+51)/(45+51+2+2)

# two var 를 사용해서...
mod_3 = glm(Republican ~ SurveyUSA + DiffCount, data=train, family=binomial)
summary(mod_3)

# aic 는 mod_3 이 가장 낮기는 하지만 p-value 가 믿음직하지 않으므로 mod_2 를 
# 사용하도록 하자.

# 그렇다면 test set 에 적용해보자.
predTest = predict(mod_2, newdata=test, type="response")
table(test$Republican, predTest > 0.5)
accuracy_test = (23+21)/nrow(test)
subset(test, Republican == 0 & predTest > 0.5)
