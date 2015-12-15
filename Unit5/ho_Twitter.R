twitter = read.csv("tweets.csv", stringsAsFactors=FALSE)
str(twitter)
twitter$Negative = as.factor(twitter$Avg <= -1)
table(twitter$Negative)

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(twitter$Tweet))
corpus
corpus[[1]]
corpus[[2]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]
corpus = tm_map(corpus, stemDocument)
corpus[[1]]

frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[101:105, 301:305])
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, sparse=0.995)
sparse
df.sparse = as.data.frame(as.matrix(sparse))
colnames(df.sparse)
colnames(df.sparse) = make.names(colnames(df.sparse))
colnames(df.sparse)
df.sparse$Negative = twitter$Negative

library(caTools)
set.seed(123)
split = sample.split(df.sparse$Negative, SplitRatio = 0.7)
train = subset(df.sparse, split == TRUE)
test = subset(df.sparse, split == FALSE)

# baseline 
tblBASE = table(test$Negative)
tblBASE
accuBase = 300/355

# CART model
library(rpart)
library(rpart.plot)
modCART = rpart(Negative ~ ., data=train, method="class")
summary(modCART)
prp(modCART)
predCART = predict(modCART, newdata=test, type="class")
tblCART = table(test$Negative, predCART)
tblCART
accuCART = (tblCART[1,1] + tblCART[2,2])/sum(tblCART)

# random forest model
library(randomForest)
modRF = randomForest(Negative ~ ., data=train)
summary(modRF)
predRF = predict(modRF, newdata=test)
tblRF = table(test$Negative, predRF)
tblRF 
accuRF = (tblRF[1,1] + tblRF[2,2])/sum(tblRF)
