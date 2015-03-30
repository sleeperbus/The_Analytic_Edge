parole = read.csv("parole.csv")
str(parole)
summary(parole)
prop.table(table(parole$violator))

parole$crime = as.factor(parole$crime)
parole$state = as.factor(parole$state)
summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == T)
test = subset(parole, split == F)

# model 1
mod1 = glm(violator ~ ., data=train, family=binomial)
summary(mod1)
