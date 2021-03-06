library(ggplot2)
library(ggmap)

intl = read.csv("intl.csv")
str(intl)

ggplot(data=intl, aes(x=Region, y=PercentOfIntl)) +
    geom_bar(stat="identity") + 
    geom_text(aes(label=PercentOfIntl))

intl = transform(intl, Region=reorder(Region, -PercentOfIntl))
intl$PercentOfIntl = intl$PercentOfIntl * 100

ggplot(data=intl, aes(x=Region, y=PercentOfIntl)) +
    geom_bar(stat="identity", fill="dark blue") + 
    geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
    ylab("Percent of International Students") + 
    theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=30, hjust=1)) 

intlall = read.csv("intlall.csv", stringsAsFactors=F)
str(intlall)
head(intlall)
tail(intlall)
mapData = map_data("world")
str(mapData)
ggplot(mapData, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") +
    coord_map("mercator")

geoData = merge(mapData, intlall, by.x="region", by.y="Citizenship")
str(geoData)

ggplot(geoData, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") + 
    coord_map("mercator")
head(geoData)

geoData = geoData[with(geoData, order(group, order)), ]
ggplot(geoData, aes(x=long, y=lat, group=group)) + 
    geom_polygon(fill="white", color="black") + 
    coord_map("ortho", orientation=c(20,30,0))

intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"

library(ggplot2)
library(reshape)

household = read.csv("households.csv")
str(household)
household[,1:2]
head(melt(household, id="Year"))
melt(household, id="Year")[1:10,3]

ggplot(data=melt(household, id="Year"), aes(x=Year, y=value, color=variable)) +
    geom_line() + geom_point(size=5)