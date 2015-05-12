library(ggplot2)

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