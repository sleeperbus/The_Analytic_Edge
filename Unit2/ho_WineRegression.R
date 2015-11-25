library(corrplot)

wine = read.csv("wine.csv", stringsAsFactors = F)
str(wine)
wine_test = read.csv("wine_test.csv", stringsAsFactors = F)
str(wine_test)

# 각 변수의 상관관계
M = cor(wine)
corrplot(M)
summary(wine)

# one variable model 
reg_1 = lm(Price ~ AGST, data = wine)
summary(reg_1)
SSE = sum(reg_1$residuals^2)
SST = sum((wine$Price - mean(wine$Price))^2)
R_2 = 1 - (SSE/SST)
RMSE = sqrt(SSE/nrow(wine))

# model for quiz 
reg_quiz = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(reg_quiz)

# model Age 는 다른 var 들과 역할이 겹치네.
reg = lm(Price ~ ., data = wine)
summary(reg)

reg = lm(Price ~ AGST + HarvestRain, data = wine)

# wine_test 로 값을 예상해보자.1980년의 Price 가 차이가 난다.
predict(reg, newdata = wine_test)
wine_test
