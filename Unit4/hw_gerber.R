gerber = read.csv("gerber.csv")
str(gerber)
prop.table(table(gerber$voting))

tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)
tapply(gerber$voting, gerber$self, mean)

# logistic model
log.mod = glm(voting ~ hawthorne + civicduty + neighbors + self,
              data = gerber, family=binomial)
summary(log.mod)
log.pred = predict(log.mod, type="response")
table(gerber$voting, log.pred > 0.3)
table(gerber$voting, log.pred > 0.5)
library(ROCR)
log.predict = prediction(log.pred, gerber$voting)
log.perf = performance(log.predict, "tpr", "fpr")
log.auc= performance(log.predict, "auc")
plot(log.perf, print.cutoffs.at=seq(0,1,0.1), colorize=T,
     text.adj=c(0, 1.7))

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(voting ~ hawthorne + civicduty + neighbors + self,
                 data=gerber)
prp(cart.mod)

cart.mod2 = rpart(voting ~ hawthorne + civicduty + neighbors + self,
                 data=gerber, cp=0.0)
prp(cart.mod2)

cart.mod3 = rpart(voting ~ hawthorne + civicduty + neighbors + self + sex,
                  data=gerber, cp=0.0)
prp(cart.mod3)


cart.control = rpart(voting ~ control, data=gerber, cp=0.0)
cart.sex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(cart.control, digit=6)
prp(cart.sex, digit=6)

log.sex = glm(voting ~ sex + control, data=gerber, family=binomial)
summary(log.sex)
possibilities = data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(log.sex, newdata=possibilities, type="response")

log.mod2 = glm(voting ~ sex + control + sex:control, data=gerber, family=binomial)
summary(log.mod2)

predict(log.mod2, newdata=possibilities, type="response")
