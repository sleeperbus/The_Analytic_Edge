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
headline = as.data.frame(as.matrix(sparseHeadline))
colnames(headline) = make.names(colnames(headline))

# Snippet
corpusSnippet= Corpus(VectorSource(news$Snippet))
dtmSnippet= DocumentTermMatrix(corpusSnippet, corpusControls)
sparseSnippet= removeSparseTerms(dtmSnippet, 0.99)
snippet = as.data.frame(as.matrix(sparseSnippet))
colnames(snippet) = make.names(colnames(snippet))

# Abstract
corpusAbstract= Corpus(VectorSource(news$Abstract))
dtmAbstract= DocumentTermMatrix(corpusAbstract, corpusControls)
sparseAbstract= removeSparseTerms(dtmAbstract, 0.99)
abstract = as.data.frame(as.matrix(sparseAbstract))
colnames(abstract) = make.names(colnames(abstract))

headline$Popular = news$Popular
headlinePopular = subset(headline, Popular==1)
headlineUnpopular = subset(headline, Popular==0)
sort(colSums(headlinePopular))
sort(colSums(headlineUnpopular))



# merge text variables
# docs = cbind(headline, snippet, abstract)
# 
# docs$Popular = news$Popular
# docs$NewsDesk = news$NewsDesk
# docs$SectionName = news$SectionName
# docs$SubsectionName = news$SubsectionName
# docs$Weekday = news$Weekday
# 
# # exploratory data
# names(docs)
# docsPopular = subset(docs, Popular==1)
# docsUnpopular = subset(docs, Popular==0)
# 
# tail(sort(colSums(docsUnpopular[1:470])), 50)
# tail(sort(colSums(docsPopular[1:470])), 50)

# Set model
spl = sample.split(docs$Popular, SplitRatio=0.7)
train = subset(docs, spl==T)
test = subset(docs, spl==F)


# CART 
cart.mod = rpart(Popular ~., data=train)
#cart.mod = rpart(Popular ~., data=train, method="class")
prp(cart.mod)
