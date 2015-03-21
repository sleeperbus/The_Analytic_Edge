data(state)
statedata = cbind(data.frame(state.x77),
                  state.abb,
                  state.area,
                  state.center,
                  state.division,
                  state.name,
                  state.region
)

str(statedata)
plot(statedata$x, statedata$y)
tapply(statedata$HS.Grad, statedata$state.region, max)
boxplot(statedata$Murder ~ statedata$state.region)
state.northeast = subset(statedata, state.region == "Northeast")
state.northeast$state.name[which.max(state.northeast$Murder)]
lm.life1= lm(Life.Exp ~ Population + Income + Illiteracy + 
                    Murder + HS.Grad + Frost + Area, data=statedata)
summary(lm.life1)
plot(statedata$Income, statedata$Life.Exp)

lm.life2 = lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, 
              data=statedata)
summary(lm.life2)
