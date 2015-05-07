news = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=F)
news$PubDate = strptime(news$PubDate, "%Y-%m-%d %H:%M:%S")
news$WeekDay = news$PubDate$wday
news$Hour = news$PubDate$hour

table(news$Popular, news$WeekDay)
table(news$Popular, news$Hour)
