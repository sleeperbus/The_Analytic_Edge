library(ggplot2)
parole = read.csv("parole.csv")
str(parole)
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

table(parole$male, parole$violator)
table(parole$state, parole$crime)

ggplot(data=parole, aes(x=age)) + geom_histogram()
ggplot(data=parole, aes(x=age)) + geom_histogram(binwidth=5) 
ggplot(data=parole, aes(x=age)) + geom_histogram(binwidth=5, color="blue")  

ggplot(data=parole, aes(x=age)) + 
  geom_histogram(binwidth=5) + facet_grid(male ~.)

ggplot(data=parole, aes(x=age)) + 
  geom_histogram(binwidth=5) + facet_grid(. ~ male)
