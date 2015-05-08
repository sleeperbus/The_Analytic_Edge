library(tm)
library(caTools)
library(rpart)
library(rpart.plot)
library(SnowballC)

news = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=F)   
news$PubDate = strptime(news$PubDate, "%Y-%m-%d %H:%M:%S")

news$Weekday = news$PubDate$wday
    
corpusControls = list(tolower=T, removePunctuation=T, 
                      stopwords=stopwords("english"),
                      stemming=function(word) wordStem(word, language="english")
                      )
# Headline
corpusHeadline = Corpus(VectorSource(news$Headline))
dtmHeadline = DocumentTermMatrix(corpusHeadline, corpusControls)
sparseHeadline = removeSparseTerms(dtmHeadline, 0.99)
headlineWords = as.data.frame(as.matrix(sparseHeadline))
colnames(headlineWords) = make.names(colnames(headlineWords))

# Snippet
corpusSnippet= Corpus(VectorSource(news$Snippet))
dtmSnippet= DocumentTermMatrix(corpusSnippet, corpusControls)
sparseSnippet= removeSparseTerms(dtmSnippet, 0.99)
snippetWords = as.data.frame(as.matrix(sparseSnippet))
colnames(snippetWords) = make.names(colnames(snippetWords))

# Abstract
corpusAbstract= Corpus(VectorSource(news$Abstract))
dtmAbstract= DocumentTermMatrix(corpusAbstract, corpusControls)
sparseAbstract= removeSparseTerms(dtmAbstract, 0.99)
abstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(abstractWords) = make.names(colnames(abstractWords))

# merge text variables
docs = cbind(headlineWords, snippetWords, abstractWords)

docs$Popular = news$Popular
docs$NewsDesk = news$NewsDesk
docs$SectionName = news$SectionName
docs$SubsectionName = news$SubsectionName
docs$Weekday = news$Weekday

#corpusHeadline = tm_map(corpusHeadline, tolower)
#corpusHeadline = tm_map(corpusHeadline, PlainTextDocument)
#corpusHeadline = tm_map(corpusHeadline, removePunctuation)
#corpusHeadline = tm_map(corpusHeadline, removeWords, stopwords("english"))
#corpusHeadline = tm_map(corpusHeadline, stemDocument)
#dtmHeadline = DocumentTermMatrix(corpusHeadline)

spl = sample.split(docs$Popular, SplitRatio=0.7)
train = subset(docs, spl==T)
test = subset(docs, spl==F)

# CART 
cart.mod = rpart(Popular ~., data=train)
#cart.mod = rpart(Popular ~., data=train, method="class")
prp(cart.mod)
