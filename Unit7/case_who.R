who = read.csv("WHO.csv", stringsAsFactors=F)
str(who)
library(ggplot2)

scatterplot = ggplot(who, aes(x=GNI, y=FertilityRate))

ggplot(who, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point()
ggplot(who, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + 
    geom_point(size=3, alpha=0.5)

ggplot(who, aes(x=FertilityRate, y=Under15)) + geom_point(size=3, alpha=0.4)
ggplot(who, aes(x=log(FertilityRate), y=Under15)) + geom_point(size=3, alpha=0.4) + stat_smooth(method="lm", level=0.99)
