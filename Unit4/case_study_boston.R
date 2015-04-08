boston = read.csv("boston.csv")
str(boston)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch=20)

summary(boston$NOX)
boxplot(boston$NOX)
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=20)

# plot prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="purple", pch=20)

# Linear Regression
vars = c("LON", "LAT", "MEDV", "CRIM", "ZN", "INDUS", "CHAS", "NOX",
         "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO")
mod1 = lm(MEDV ~ LON + LAT, data=boston)
summary(mod1)
plot(boston$LON, boston$LAT)
points(boston$LON[mod1$fitted.value >= 21.2], 
       boston$LAT[mod1$fitted.value >= 21.2],
       col="red", pch=20)

# CART 
library(rpart)
library(rpart.plot)
tree.mod = rpart(MEDV ~ LON + LAT, data=boston)
prp(tree.mod)
tree.pred = predict(tree.mod)
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="purple", pch=20)
points(boston$LON[tree.pred>=21.2], boston$LAT[tree.pred>=21.2], col="red", pch="$")

# simple CART
tree.mod = rpart(MEDV ~ LON + LAT, data=boston, minbucket=50)
plot(tree.mod)
text(tree.mod)
prp(tree.mod)
plot(boston$LON, boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)

# again...
library(caTools)
set.seed(123)
form = MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE +
  DIS + RAD + TAX + PTRATIO
split = sample.split(boston$MEDV, SplitRatio=0.7)
train = subset(boston, split==T)
test = subset(boston, split==F)
lin.mod = lm(form, data=train)
summary(lin.mod)
lin.pred = predict(lin.mod, newdata=test)
lin.sse = sum((lin.pred - test$MEDV)^2)
lin.sse

tree.mod = rpart(form, data=train)
prp(tree.mod)
plot(tree.mod)
text(tree.mod)
tree.pred = predict(tree.mod, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

# Cross Validation
library(caret)
library(e1071)

tr.control = trainControl(method="cv", number = 10)
cp.grid = expand.grid(.cp=(0:10)*0.001)
tr = train(form, data=train, method="rpart", trControl = tr.control, tuneGrid = cp.grid) 
cv.pred = predict(tr$finalModel, newdata=test)
cv.sse = sum((test$MEDV - cv.pred)^2)
cv.sse
