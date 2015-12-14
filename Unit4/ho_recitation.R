boston = read.csv("boston.csv")
str(boston)
summary(boston)
summary(boston$MEDV)
boxplot(boston$MEDV)
hist(boston$MEDV, breaks=30)

# correlation 
library(corrplot)
dfCor = boston[, c(2:ncol(boston))]
M = cor(dfCor)
corrplot.mixed(M)

# 평균보다 높은 가격의 집들
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV > 22], boston$LAT[boston$MEDV > 22], col="forestgreen", pch=19)

# 범죄율
summary(boston$CRIM)
boxplot(boston$CRIM)
boxplot(log(boston$CRIM))
hist(boston$CRIM, breaks=40)
hist(log(boston$CRIM), breaks=40)


# linear model 
modLin = lm(MEDV ~ ., data=boston)
summary(modLin)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=20)
points(boston$LON[modLin$fitted.values >= 21.2], boston$LAT[modLin$fitted.values >= 21.2], col="blue", pch="$")

modLin2 = lm(MEDV ~ . - TOWN - TRACT, data=boston)
summary(modLin2)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=20)
points(boston$LON[modLin2$fitted.values >= 21.2], boston$LAT[modLin2$fitted.values >= 21.2], col="blue", pch="$")

library(rpart)
library(rpart.plot)
modCART = rpart(MEDV ~ LAT + LON, data=boston)
fittedCART = predict(modCART)
prp(modCART)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col="red", pch=20)
points(boston$LON[fittedCART >= 21.2], boston$LAT[fittedCART >= 21.2], col="blue", pch="$")

# simplify
modCART2 = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(modCART2)
text(modCART2)

library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

# linear model
modLin = lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(modLin)
predLin = predict(modLin, newdata=test)
sseLin = sum((test$MEDV - predLin)^2)

# CART model
modCART = rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(modCART)
summary(modCART)
predCART = predict(modCART, newdata=test)
sseCART = sum((test$MEDV - predCART)^2)

# randomForest 
library(randomForest)
modRF = randomForest(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
print(modRF)
summary(modRF)
predRF = predict(modRF, newdata=test)
sseRF = sum((test$MEDV - predRF)^2)

library(caret)
library(e1071)
trCtrl = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=(0:10)*0.001) 
tr = train(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train,
           method="rpart", trControl=trCtrl, tuneGrid=cpGrid)
prp(tr$finalModel)
predCV = predict(tr$finalModel, newdata=test)
sseCV = sum((test$MEDV - predCV)^2)
