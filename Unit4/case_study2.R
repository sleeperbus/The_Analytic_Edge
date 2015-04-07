claims = read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio=0.6)
train = subset(claims, split==T)
test = subset(claims, split==F)

# base line 
sum(diag(1, 5) * table(test$bucket2009, test$bucket2008))/nrow(test)
penaltyMatrix  = matrix(c(0, 1, 2, 3, 4, 2, 0 , 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 
                         0, 1, 8, 6, 4, 2, 0), nrow=5, ncol=5, byrow=T)
sum(as.matrix(table(test$bucket2009, test$bucket2008)) * penaltyMatrix)/nrow(test)
