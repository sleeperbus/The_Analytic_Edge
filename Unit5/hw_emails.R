emails = read.csv("emails.csv", stringsAsFactors=FALSE)
str(emails)
table(emails$spam)

# baseline model
4360/(4360+1368)

library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, 0.95)
emailSparse = as.data.frame(as.matrix(spdtm))
names(emailSparse) = make.names(names(emailSparse), unique=T)

sort(colSums(emailSparse))

emailSparse$spam = emails$spam

sort(colSums(subset(emailSparse, spam==0)))
sort(colSums(subset(emailSparse, spam==1)))
