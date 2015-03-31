loans = read.csv("loans.csv")
str(loans)
summary(loans)
prop.table(table(loans$not.fully.paid))
