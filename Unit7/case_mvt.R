library(ggplot2)
mvt = read.csv("mvt.csv", stringsAsFactors=F)
str(mvt)
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

mvt$Weekday = factor(mvt$Weekday, ordered=T, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
weekdayCount = as.data.frame(table(mvt$Weekday))
str(weekdayCount)
colnames(weekdayCount) = c("Day", "Freq")
plot(weekdayCount$Day, weekdayCount$Freq)
ggplot(weekdayCount, aes(x=Day, y=Freq)) + geom_point()
ggplot(weekdayCount, aes(x=Day, y=Freq)) + geom_line(aes(group=1)) + 
    xlab("Day of the week") + ylab("Total motor vehicle Thefts")

dayHourCount = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(dayHourCount)
dayHourCount$Var2 = as.numeric(as.character(dayHourCount$Var2))
dayHourCount$Type = ifelse(dayHourCount$Var1 %in% c("Sunday", "Saturday"),
                            "Weekend", "Weekday")
names(dayHourCount) = c("Day", "Hour", "Freq", "Type")
ggplot(dayHourCount, aes(x=Hour, y=Freq)) + 
    geom_line(aes(color=Day), size=2, alpha=0.5) +
    xlab("Hour") + ylab("Freq")
ggplot(dayHourCount, aes(x=Hour, y=Freq)) + 
    geom_line(aes(gruop=Day, color=Type), size=2, alpha=0.5) 

ggplot(dayHourCount, aes(x=Hour, y=Day)) + 
    geom_tile(aes(fill=Freq)) +
    scale_fill_gradient(name="Total MV Thefts", low="white", high="red") +
    theme(axis.title.y=element_blank())

library(maps)
library(ggmap)

chicago = get_map(location="chicago", zoom=11)
ggmap(chicago)
geoData = as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude,2)))
str(geoData)
geoData$Var1 = as.numeric(as.character(geoData$Var1))
geoData$Var2 = as.numeric(as.character(geoData$Var2))
colnames(geoData) = c("Long", "Lat", "Freq")

ggmap(chicago) + 
    geom_point(data=geoData, aes(x=Long, y=Lat, color=Freq, size=Freq)) +
    scale_colour_gradient(low="yellow", high="red") +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank())


ggmap(chicago) + 
    geom_tile(data=geoData, aes(x=Long, y=Lat, alpha=Freq), fill="red")


murders = read.csv("murders.csv")
str(murders)
statesMap = map_data("state")
class(statesMap)
str(statesMap)
ggplot(data=statesMap, aes(x=long,y=lat)) + geom_point()
murders$region= tolower(murders$State)
ggplot(data=statesMap, aes(x=long,y=lat,group=group)) + 
    geom_polygon(fill="white", color="black")
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

ggplot(data=murderMap, aes(x=long, y=lat, group=group, fill=Population)) +
    geom_polygon(color="black") +
    scale_fill_gradient(low="yellow", high="red", guide="legend")

murderMap$MurderRate = murderMap$Murders / murderMap$Population * 10000
ggplot(data=murderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) +
    geom_polygon(color="black", size=0.5) + 
    scale_fill_gradient(low="black", high="red", limits=c(0,1))
