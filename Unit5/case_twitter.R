tweets = read.csv("tweets.csv", stringsAsFactors=F)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
prop.table(table(tweets$Negative))

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus= tm_map(corpus, stemDocument, lazy = TRUE)

frequencies = DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)

tweetSparse = as.data.frame(as.matrix(sparse))
colnames(tweetSparse) = make.names(colnames(tweetSparse))
tweetSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetSparse$Negative, SplitRatio = 0.7)
train = subset(tweetSparse, split==T)
test = subset(tweetSparse, split==F)

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(Negative ~ ., data=train, method="class")
prp(cart.mod)
cart.pred = predict(cart.mod, newdata=test, type="class")
cart.table = table(test$Negative, cart.pred)
sum(diag(1,2)*cart.table)/sum(cart.table)

# base line
300/(300+55)

# random forest
library(randomForest)
set.seed(123)
forest.mod = randomForest(Negative ~ ., data=train)
forest.pred = predict(forest.mod, newdata=test)
forest.table = table(test$Negative, forest.pred)
forest.table
sum(diag(1,2)*forest.table)/sum(forest.table)

