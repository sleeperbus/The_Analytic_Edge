claims = read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio=0.6)
train = subset(claims, split==T)
test = subset(claims, split==F)

