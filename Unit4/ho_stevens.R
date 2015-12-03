stevens = read.csv("stevens.csv")
str(stevens)

# baseline 설정
table(stevens$Reverse)
baseline = 309/nrow(stevens)
# 무조건 reverse 라고 하면 반은 맞는다. 

# 데이터를 나누자.
library(caTools)
set.seed(332)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)
train = subset(stevens, split == TRUE)
test = subset(stevens, split == FALSE)

form = Reverse ~ Term + Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst

# logistic regression 으로...
modLog = glm(form, data=train, family=binomial)
summary(modLog)
