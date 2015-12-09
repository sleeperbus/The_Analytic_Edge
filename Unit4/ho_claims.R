claims = read.csv("ClaimsData.csv")
str(claims)

# 비율을 보자 
table(claims$bucket2008)
prop.table(table(claims$bucket2008))
# bucket1 이 역시나 가장 많고... 이후부터 급격히 줄어듬. 대부분의 사람들은 
# 저위험군에 속한다. 

# baseline 을 구해보자. 다른 요소들은 고려하지 않고 2009년의 결과 그대로 
# 2009년이 나온다 가정한다면...
tblBase = table(claims$bucket2009, claims$bucket2008)
tblBase
accuBase = sum(tblBase * diag(1, 5))/sum(tblBase)

# 잘못 예측한 것에 대한 penalty matrix 를 만듭시다. 
penaltyMat = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
penaltyBase = sum(as.matrix(tblBase) * penaltyMat) / nrow(claims)
# penalty 는 0.73 보다는 작아야 한다.

# logistic regression 은 불가능하니 CART model 부터...

library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio = 0.7)
train = subset(claims, split == TRUE)
test = subset(claims, split == FALSE)

# CART model 
modCART = rpart