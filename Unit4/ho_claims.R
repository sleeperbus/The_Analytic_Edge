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
split = sample.split(claims$bucket2009, SplitRatio = 0.6)
train = subset(claims, split == TRUE)
test = subset(claims, split == FALSE)

# CART model 
library(rpart)
library(rpart.plot)
modCART = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + 
                  depression + diabetes + heart.failure + ihd + kidney + 
                  osteoporosis + stroke + bucket2008 + reimbursement2008, 
                data=train, method="class", cp=0.00005)
prp(modCART)
# 엄청나게 큰 트리가 나온다. 결과값이나 봅시다. 
predCART = predict(modCART, newdata=test, type="class")  
tblCART = table(test$bucket2009, predCART)
tblCART
accuCART = sum(tblCART * diag(1,5))/sum(tblCART)
penaltyCART = sum(as.matrix(tblCART) * penaltyMat)/nrow(test)
# penaltyCART 가 penaltyBase 보다 크게 나온다. modCART 를 만들 때 penalty 를 고려하지
# 않고 만들었기 때문이다. 
modCART2 = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + 
                  depression + diabetes + heart.failure + ihd + kidney + 
                  osteoporosis + stroke + bucket2008 + reimbursement2008, 
                data=train, method="class", cp=0.00005, 
                parms=list(loss=penaltyMat))
predCART2 = predict(modCART2, newdata=test, type="class")  
tblCART2 = table(test$bucket2009, predCART2)
tblCART2
accuCART2 = sum(tblCART2 * diag(1,5))/sum(tblCART2)
penaltyCART2 = sum(as.matrix(tblCART2) * penaltyMat)/nrow(test)
# 이렇게 되면 penalty 는 떨어지지만 정확도도 떨어져버린다. 

