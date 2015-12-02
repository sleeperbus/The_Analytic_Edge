baseball = read.csv("baseball.csv")
str(baseball)
length(table(baseball$Year))
baseball = subset(baseball, Playoffs == 1)
playoffTable = table(baseball$Year)
names(playoffTable)
baseball$NumCompetitors = playoffTable[as.character(baseball$Year)]
nrow(subset(baseball, NumCompetitors == 8))
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
sum(baseball$WorldSeries == 0)

sumArr = rep(0, 100)
mod_1 = glm(WorldSeries ~ Year, data=baseball, family=binomial)
summary(mod_1)

mod_2 = glm(WorldSeries ~ RS, data=baseball, family=binomial)
summary(mod_2)
summary(mod_2)

mod_3 = glm(WorldSeries ~ RA, data=baseball, family=binomial)
summary(mod_3)

mod_4 = glm(WorldSeries ~ W, data=baseball, family=binomial)
summary(mod_4)

mod_5 = glm(WorldSeries ~ OBP, data=baseball, family=binomial)
summary(mod_5)

mod_6 = glm(WorldSeries ~ SLG, data=baseball, family=binomial)
summary(mod_6)

mod_7 = glm(WorldSeries ~ BA, data=baseball, family=binomial)
summary(mod_7)

mod_8 = glm(WorldSeries ~ RankSeason, data=baseball, family=binomial)
summary(mod_8)

mod_9 = glm(WorldSeries ~ OOBP, data=baseball, family=binomial)
summary(mod_9)

mod_10 = glm(WorldSeries ~ OSLG, data=baseball, family=binomial)
summary(mod_10)

mod_11 = glm(WorldSeries ~ NumCompetitors, data=baseball, family=binomial)
summary(mod_11)

mod_12 = glm(WorldSeries ~ League, data=baseball, family=binomial)
summary(mod_12)

logMod = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data=baseball, family=binomial)
summary(logMod)

library(corrplot)
M = cor(baseball[, c("NumCompetitors", "RA", "RankSeason", "Year")])
corrplot.mixed(M)

mod_a1 = glm(WorldSeries ~ NumCompetitors + RA, data=baseball, family=binomial)
summary(mod_a1)

mod_a2 = glm(WorldSeries ~ NumCompetitors + RankSeason, data=baseball, family=binomial)
summary(mod_a2)

mod_a3 = glm(WorldSeries ~ NumCompetitors + Year, data=baseball, family=binomial)
summary(mod_a3)

mod_a4 = glm(WorldSeries ~ RA + RankSeason, data=baseball, family=binomial)
summary(mod_a4)

mod_a5 = glm(WorldSeries ~ RA + Year, data=baseball, family=binomial)
summary(mod_a5)

mod_a6 = glm(WorldSeries ~ RankSeason + Year, data=baseball, family=binomial)
summary(mod_a6)
