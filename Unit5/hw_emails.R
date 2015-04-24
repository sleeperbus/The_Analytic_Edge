emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

# baseline model
4360/(4360+1368)

library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
emailSparse = as.data.frame(as.matrix(spdtm))
names(emailSparse) = make.names(names(emailSparse), unique=T)

sort(colSums(emailSparse))

emailSparse$spam = emails$spam

sort(colSums(subset(emailSparse, spam==0)))
sort(colSums(subset(emailSparse, spam==1))) 

emailSparse$spam = as.factor(emailSparse$spam)

library(caTools)
set.seed(123)
split = sample.split(emailSparse$spam, SplitRatio=0.7)
train = subset(emailSparse, split==T)
test = subset(emailSparse, split==F)

# Logistic Regression
spamLog = glm(spam ~., data=train, family=binomial)
summary(spamLog)

# CART model
library(rpart)
library(rpart.plot)
spamCART = rpart(spam ~., data=train, method="class")
prp(spamCART)

# Random Forest Model
library(randomForest)
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

# train set occuracy
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

tableTrainLog = table(train$spam, predTrainLog > 0.5)
sum(diag(1,2)*tableTrainLog)/sum(tableTrainLog)
tableTrainCART = table(train$spam, predTrainCART > 0.5)
sum(diag(1,2)*tableTrainCART)/sum(tableTrainCART)
tableTrainRF= table(train$spam, predTrainRF> 0.5)
sum(diag(1,2)*tableTrainRF)/sum(tableTrainRF)

library(ROCR)
predictionTrainLog = prediction(predTrainLog, train$spam)
aucTrainLog = performance(predictionTrainLog, "auc")@y.values
predictionTrainCART = prediction(predTrainCART, train$spam)
aucTrainCART = performance(predictionTrainCART, "auc")@y.values
predictionTrainRF= prediction(predTrainRF, train$spam)
aucTrainRF= performance(predictionTrainRF, "auc")@y.values

# Test set occuracy
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]

tableTestLog = table(test$spam, predTestLog > 0.5)
sum(diag(1,2)*tableTestLog)/sum(tableTestLog)
tableTestCART = table(test$spam, predTestCART > 0.5)
sum(diag(1,2)*tableTestCART)/sum(tableTestCART)
tableTestRF= table(test$spam, predTestRF> 0.5)
sum(diag(1,2)*tableTestRF)/sum(tableTestRF)

predictionTestLog = prediction(predTestLog, test$spam)
aucTestLog = performance(predictionTestLog, "auc")@y.values
predictionTestCART = prediction(predTestCART, test$spam)
aucTestCART = performance(predictionTestCART, "auc")@y.values
predictionTestRF= prediction(predTestRF, test$spam)
aucTestRF= performance(predictionTestRF, "auc")@y.values

library(slam)
wordCount = rollup(dtm, 2, FUN=sum)$v

emailSparse$logWordCount = log(wordCount)
boxplot(logWordCount ~ spam, data=emailSparse)

train2 = subset(emailSparse, split==T)
test2 = subset(emailSparse, split==F)

# CART
spam2CART = rpart(spam ~., data=train2)

# RF 
set.seed(123)
spam2RF = randomForest(spam ~., data=train2)

# predict 
predTest2CART = predict(spam2CART, newdata=test2)[,2]
tableTest2CART = table(test2$spam, predTest2CART > 0.5)
sum(diag(1,2)*tableTest2CART)/sum(tableTest2CART)

predTest2RF = predict(spam2RF, newdata=test2, type="prob")[,2]
tableTest2RF= table(test2$spam, predTest2RF> 0.5)
sum(diag(1,2)*tableTest2RF)/sum(tableTest2RF)
