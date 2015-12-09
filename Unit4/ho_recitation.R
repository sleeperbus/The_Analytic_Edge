boston = read.csv("boston.csv")
str(boston)
summary(boston)
summary(boston$MEDV)
boxplot(boston$MEDV)
hist(boston$MEDV, breaks=30)

plot(boston$LON, boston$LAT)

library(caTools)
set.seed(88)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)
# linear model 
modLin1 = lm(MEDV ~ . - TOWN, data=train)
summary(modLin1)
predTest = predict(modLin1, newdata=test)
