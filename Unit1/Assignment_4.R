poll = read.csv("AnonymityPoll.csv")
summary(poll)
str(poll)
table(poll$Smartphone)
summary(poll$Smartphone)
table(poll$Sex, poll$Region)
table(poll$State, poll$Region)
table(poll$Internet.Use, poll$Smartphone)

limited = subset(poll, Internet.Use == 1 | Smartphone == 1)
summary(limited)
str(limited)
table(limited$Info.On.Internet)
table(limited$Worry.About.Info)

hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet, xlab="Age")
boxplot(limited$Age ~ limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, summary)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
