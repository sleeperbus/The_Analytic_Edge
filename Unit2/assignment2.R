data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,
                  state.division, state.name, state.region)
# 미국 모양이 나온다.
plot(statedata$x, statedata$y)

# 지역구분에 따른 고등학교 졸업률
tapply(statedata$HS.Grad, statedata$state.region, mean, na.rm=T)

# 지역에 따른 살인율?
boxplot(statedata$Murder ~ statedata$state.region)

# Northeast 지역에 outlier 가 있다. 
subset(statedata, state.region == "Northeast")

# Life.Exp 를 산출하는 linear model
reg_life = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad +
                Frost + Area, data=statedata)
summary(reg_life)
# Income 이 늘어날수록 Life.Exp 는 줄어든다? 어차피 siginificant var 가 아니라서
# 없애면 되는 변수이긴 한데...
plot(statedata$Income, statedata$Life.Exp)
# multicollinearity 때문인 듯.. 

# 그렇다면 변수를 줄여보자.
reg_life2 = lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad +
                Frost , data=statedata)
summary(reg_life2)

reg_life3 = lm(Life.Exp ~ Population + Income + Murder + HS.Grad + Frost, data=statedata)
summary(reg_life3)

reg_life4 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data=statedata)
summary(reg_life4)

reg_life5 = lm(Life.Exp ~ Murder + HS.Grad + Frost, data=statedata)
summary(reg_life5)

# refine model 로 추정해보자.
predictionLife = predict(reg_life4)
sort(predictionLife)
# Alabama 가 가장 낮다고 예측된다.
statedata[which.min(statedata$Life.Exp), ]
statedata[which.max(statedata$Life.Exp), ]
# 실제로는 South Crolina 가 가장 낮다.
sort(abs(reg_life4$residuals))








