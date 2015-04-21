wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved= tm_map(corpusRemoved, stemDocument)
dtmRemoved= DocumentTermMatrix(corpusRemoved)
sparseRemoved= removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved= as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio=0.7)
train = subset(wikiWords, split==T)
test = subset(wikiWords, split==F)

# base line
table(test$Vandal)
618/(618+545)

# CART model
library(rpart)
library(rpart.plot)
cart.mod = rpart(Vandal ~., data=train, method="class")
prp(cart.mod)
cart.pred = predict(cart.mod, newdata=test)
cart.table = table(test$Vandal, cart.pred[,2] > 0.5)
cart.table
sum(diag(1,2)*cart.table)/sum(cart.table)

wikiWords2 = wikiWords
wikiWords2$HTTP =  ifelse(grepl("http", wiki$Added, fixed=T), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==T)
wikiTest2 = subset(wikiWords2, split==F)

cart.mod2 = rpart(Vandal~., data=wikiTrain2, method="class")
prp(cart.mod2)
cart.pred2 = predict(cart.mod2, newdata=wikiTest2)
cart.table2 = table(wikiTest2$Vandal, cart.pred2[,2] > 0.5)
sum(diag(1,2)*cart.table2)/sum(cart.table2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved= rowSums(as.matrix(dtmRemoved))
summary(wikiWords2$NumWordsAdded)

train3 = subset(wikiWords2, split==T)
test3 = subset(wikiWords2, split==F)

cart.mod3 = rpart(Vandal ~., data=train3, method="class")
prp(cart.mod3)
cart.pred3 = predict(cart.mod3, newdata=test3, type="class")
cart.table3 = table(test3$Vandal, cart.pred3)
sum(diag(1,2)*cart.table3)/sum(cart.table3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train4 = subset(wikiWords3, split==T)
test4 = subset(wikiWords3, split==F)
cart.mod4 = rpart(Vandal ~., data=train4, method="class")
prp(cart.mod4)
cart.pred4 = predict(cart.mod4, newdata=test4, type="class")
cart.table4 = table(test4$Vandal, cart.pred4)
sum(diag(1,2)*cart.table4)/sum(cart.table4)
