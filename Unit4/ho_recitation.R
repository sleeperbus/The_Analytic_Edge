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

# 데이터 분리
library(caTools)
set.seed(8)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

# linear model 
modLin1 = lm(MEDV ~ . - TRACT, data=train)
summary(modLin1)





predTest = predict(modLin1, newdata=test)
