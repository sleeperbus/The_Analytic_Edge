baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)
length(table(baseball$Year))

baseball = subset(baseball, Playoffs==1)
table(baseball$Year)
PlayoffTable = table(baseball$Year)
names(PlayoffTable)

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
table(baseball$NumCompetitors)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)
