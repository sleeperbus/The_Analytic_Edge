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
