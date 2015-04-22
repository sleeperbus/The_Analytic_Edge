clinical = read.csv("clinical_trial.csv", stringsAsFactors=F)
str(clinical)
summary(clinical)
table(clinical$trial)
summary(nchar(clinical$abstract))
sum(nchar(clinical$abstract) == 0)
clinical$title[which.min(nchar(clinical$title))]

# Make corpus
library(tm)
corpusTitle = Corpus(VectorSource(clinical$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

corpusAbstract = Corpus(VectorSource(clinical$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract= removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinical$trial

# split data
library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio=0.7)
train = subset(dtm, split==T)
test = subset(dtm, split==F)

# base line accuracy for training set
table(train$trial)
730/(730+572)

# CART model
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)
trialPred = predict(trialCART, data=train$trial)
trialTable = table(train$trial, trialPred[,2] > 0.5)
sum(diag(1,2)*trialTable)/sum(trialTable)


trialPred = predict(trialCART, newdata=test)
trialTable = table(test$trial, trialPred[,2] > 0.5)
sum(diag(1,2)*trialTable)/sum(trialTable)

library(ROCR)
trial.pred = prediction(trialPred[,2], test$trial)
trial.perfomance = performance(trial.pred, "auc")
trial.perfomance@y.values
