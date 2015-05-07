library(tm)
library(caTools)
library(rpart)
library(rpart.plot)

news = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=F)
corpusHeadline = Corpus(VectorSource(news$Headline))
corpusHeadline = tm_map(corpusHeadline, tolower)
corpusHeadline = tm_map(corpusHeadline, PlainTextDocument)
corpusHeadline = tm_map(corpusHeadline, removePunctuation)
corpusHeadline = tm_map(corpusHeadline, removeWords, stopwords("english"))
corpusHeadline = tm_map(corpusHeadline, stemDocument)
dtmHeadline = DocumentTermMatrix(corpusHeadline)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.99)

headlineWords = as.data.frame(as.matrix(sparseHeadline))
colnames(headlineWords) = make.names(colnames(headlineWords))
colMeans(headlineWords)
headlineWords$Popular = news$Popular

spl = sample.split(headlineWords$Popular, SplitRatio=0.7)
train = subset(headlineWords, spl==T)
test = subset(headlineWords, spl==F)

# CART 
cart.mod = rpart(Popular ~., data=train)
prp(cart.mod)
