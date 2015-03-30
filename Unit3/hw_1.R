songs = read.csv("songs.csv")
summary(songs)
str(songs)
table(songs$year)
michael = subset(songs, songs$artistname == "Michael Jackson")
michael$songtitle[which(michael$Top10 == 1)]

table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

train = subset(songs, year <= 2009)
test = subset(songs, year >= 2010)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[, !(names(train) %in% nonvars)]
test = test[, !(names(test) %in% nonvars)]

# model 1
mod1 = glm(Top10 ~ ., data=train, family=binomial)
summary(mod1)
