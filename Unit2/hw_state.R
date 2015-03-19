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
lm.lifeexp = lm(Life.Exp ~ Population + Income + Illiteracy + 
                    Murder + HS.Grad + Frost + Area, data=statedata)
summary(lm.lifeexp)
