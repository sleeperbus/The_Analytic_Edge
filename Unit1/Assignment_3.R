CPS = read.csv("CPSData.csv")
summary(CPS)
table(CPS$Race, CPS$Hispanic)
str(CPS)
sort(table(CPS$Industry))
sort(table(CPS$State))
table(CPS$Race, CPS$Hispanic)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroAreaCode))
table(as.vector(CPS$Region), is.na(CPS$MetroAreaCode))
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
summary(MetroAreaMap)
str(MetroAreaMap)
summary(CountryMap)
str(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)
sort(table(CPS$MetroArea), TRUE)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sum(tapply(CPS$Race == "Asian", CPS$MetroArea, mean, na.rm = TRUE) > 0.2, na.rm = TRUE)
sort(tapply(CPS$Education == "No high school diploma", 
            CPS$MetroArea, mean, na.rm=TRUE), TRUE)

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
names(CPS)
summary(CPS)

tapply(CPS$Country != "United States", CPS$MetroArea, mean, na.rm=TRUE)
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", 
    CPS$Country != "United States")

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
