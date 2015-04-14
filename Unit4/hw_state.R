data(state)
statedata = data.frame(state.x77)
str(statedata)

# full linear regression model
lin.mod = lm(Life.Exp ~ Population + Income + Illiteracy + Murder +
                 HS.Grad + Frost + Area, data=statedata)
summary(lin.mod)
lin.sse = sum((statedata$Life.Exp - lin.mod$fitted.values)^2)
lin.sse

# using sub vars for linear regression model
lin.sub.mod = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,
                 data=statedata)
summary(lin.sub.mod)
lin.sub.sse = sum((statedata$Life.Exp - lin.sub.mod$fitted.values)^2)
lin.sub.sse

vars = c("Life.Exp", "Income", "Illiteracy", "Area")
cor(statedata[vars])

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(Life.Exp ~ Population + Income + Illiteracy + Murder +
                     HS.Grad + Frost + Area, data=statedata)
prp(cart.mod)
cart.pred = predict(cart.mod)
cart.sse = sum((statedata$Life.Exp - cart.pred)^2)
cart.sse

# Another CART model
cart.mod2 = rpart(Life.Exp ~ Population + Income + Illiteracy + Murder +
                     HS.Grad + Frost + Area, data=statedata, minbucket=5)
prp(cart.mod2)
cart.pred2 = predict(cart.mod2)
cart.sse2 = sum((statedata$Life.Exp - cart.pred2)^2)
cart.sse2

# Another CART model
cart.mod3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
prp(cart.mod3)
cart.pred3 = predict(cart.mod3)
cart.sse3 = sum((statedata$Life.Exp - cart.pred3)^2)
cart.sse3

# Cross Validation
library(caret)
library(e1071)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = data.frame(expand.grid(.cp = seq(0.01, 0.5, 0.01) ))
tr = train(Life.Exp ~ ., data=statedata, 
          method="rpart", trControl = fitControl, tuneGrid = cartGrid)

# CART model with cp value
cart.cp.mod = rpart(Life.Exp ~ ., data=statedata, cp=0.12)
prp(cart.cp.mod)
cart.cp.pred = predict(cart.cp.mod)
cart.cp.sse = sum((statedata$Life.Exp - cart.cp.pred)^2)
cart.cp.sse

# 
set.seed(111)
fitControl = trainControl(method = "cv", number = 10)
cartGrid = data.frame(expand.grid(.cp = seq(0.01, 0.5, 0.01) ))
tr=train(Life.Exp ~ Area, data=statedata, method="rpart",
         trControl=fitControl, tuneGrid=cartGrid)