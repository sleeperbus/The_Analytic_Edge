email = read.csv("energy_bids.csv")
str(email)
email$email[1]

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(email$email))
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus[[1]]
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]][1]
corpus = tm_map(corpus, stemDocument)
corpus[[1]][1]

dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm[100:105, 4001:4005])
findFreqTerms(dtm, lowfreq = 20)
findFreqTerms(dtm, lowfreq = 40)
findFreqTerms(dtm, lowfreq = 100)

sparse = removeSparseTerms(dtm, 0.97)
sparse
df = as.data.frame(as.matrix(sparse))
str(df)
head(df)
colnames(df) = make.names(colnames(df))
df$responsive = email$responsive

library(caTools)
set.seed(144)
split = sample.split(df$responsive, SplitRatio = 0.7)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

# baseline 
tblBase = table(test$responsive)
tblBase 
accuBase = 215/sum(tblBase)

# CART model 
library(rpart)
library(rpart.plot)
modCART = rpart(responsive ~ ., data=train, method="class")
prp(modCART)
predCART = predict(modCART, newdata=test, type="class")
tblCART = table(test$responsive, predCART)
tblCART
accuCART = (tblCART[1,1]+tblCART[2,2])/sum(tblCART)

# ROC 커브를 봅시다. 
library(ROCR)
predCART2 = predict(modCART, newdata=test)
predCART2
predROCR = prediction(predCART2[,2], test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(0, -1.5))

# AUC 
performance(predROCR, "auc")@y.values

# random forest model
library(randomForest)
train$responsive = as.factor(train$responsive)
test$responsive = as.factor(test$responsive)
modRF = randomForest(responsive ~ ., data=train)
predRF = predict(modRF, newdata = test)
tblRF = table(test$responsive, predRF)
tblRF
accuRF = (tblRF[1,1]+tblRF[2,2])/sum(tblRF)


