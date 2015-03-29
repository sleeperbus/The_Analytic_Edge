songs = read.csv("songs.csv")
summary(songs)
str(songs)
table(songs$year)
michael = subset(songs, songs$artistname == "Michael Jackson")
michael$songtitle[which(michael$Top10 == 1)]

table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]
