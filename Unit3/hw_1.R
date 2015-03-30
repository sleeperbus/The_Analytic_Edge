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
cor(train$loudness, train$energy)

# model 2
mod2 = glm(Top10 ~ . -loudness, data=train, family=binomial)
summary(mod2)

# model 3
mod3 = glm(Top10 ~ . -energy, data=train, family=binomial)
summary(mod3)

# predict
pred3 = predict(mod3, newdata=test, type="response")
prop.table(table(test$Top10, pred3 > 0.45))
library(ROCR)
ROCRpred = prediction(pred3, test$Top10)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=T, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(0, 1.7))

# base model
prop.table(table(test$Top10))
