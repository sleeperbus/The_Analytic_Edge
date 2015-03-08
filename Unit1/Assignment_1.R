mvt = read.csv("mvtWeek1.csv")
nrow(mvt)
names(mvt)
str(mvt)
head(mvt)
max(mvt$ID)
min(mvt$Beat)
length(which(mvt$Arrest == TRUE))
match("ALLEY", mvt$LocationDescription)
table(mvt$LocationDescription)
mvt$Date[1]
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
attach(mvt)
table(mvt$Month)
which.min(table(mvt$Month))
which.max(table(mvt$Weekday))
table(Arrest, Month)
hist(Date, 100)
boxplot(Date ~ Arrest)
mvt.2001 = subset(mvt, Year==2001)
prop.table(table(mvt.2001$Arrest))
mvt.2007 = subset(mvt, Year==2007)
prop.table(table(mvt.2007$Arrest))
mvt.2012 = subset(mvt, Year==2012)
prop.table(table(mvt.2012$Arrest))
sort(table(mvt$LocationDescription), TRUE)[1:6]
Top5 = subset(mvt, LocationDescription %in% (c(
        "STREET", 
        "PARKING LOT/GARAGE(NON.RESID.)", 
        "ALLEY",
        "GAS STATION",
        "DRIVEWAY - RESIDENTIAL"
        
        )
    ))
Top5$Weekday = factor(Top5$Weekday)
summary(Top5)
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)
table(Top5$LocationDescription, Top5$Weekday)
