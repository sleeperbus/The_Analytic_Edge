edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(edges)
str(users)

table(users$locale)
table(users$gender, users$locale, useNA="no")
