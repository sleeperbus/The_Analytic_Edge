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
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)

tweetSparse = as.data.frame(as.matrix(sparse))
tweetSparse$Negative = tweets$Negative

