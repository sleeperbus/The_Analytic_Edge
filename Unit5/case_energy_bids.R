emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$responsive)
716/(716+139)

library(tm)
corpus = Corpus(VectorSource(emails$email))
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, sparse = 0.97)

terms = as.data.frame(as.matrix(dtm))
terms$responsive = emails$responsive
str(terms)

library(caTools)
set.seed(144)
split = sample.split(terms$responsive, SplitRatio=0.7)
train = subset(terms, split==T)
test = subset(terms, split==F)

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(responsive ~ ., data=train, method="class")
prp(cart.mod)
cart.pred = predict(cart.mod, newdata=test)
cart.table = table(test$responsive, cart.pred[,2] > 0.5)
sum(diag(1,2)*cart.table)/sum(cart.table)

library(ROCR)
cart.prediction = prediction(cart.pred[,2], test$responsive)
cart.perf = performance(cart.prediction, "tpr", "fpr")
plot(cart.perf, colorize=T, print.cutoffs.at=seq(0,1,0.1), text.adj=c(0,1.7))
