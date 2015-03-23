elantra = read.csv("elantra.csv")
summary(elantra)
training = subset(elantra, Year <= 2012)
test = subset(elantra, Year >= 2013)

lm.elan1 = lm(ElantraSales ~ Unemployment+Queries+CPI_energy+CPI_all,
              data=training)
summary(lm.elan1)

lm.elan2 = lm(ElantraSales ~ Month+Unemployment+Queries+CPI_energy+CPI_all,
              data=training)
summary(lm.elan2)

training$MonthFactor= as.factor(training$Month)
test$MonthFactor = as.factor(test$Month)
lm.elan2 = lm(ElantraSales ~ MonthFactor+Unemployment+Queries+CPI_energy+CPI_all,
              data=training)
summary(lm.elan2)

cor(training[c("Month", "Unemployment", "Queries", "CPI_energy",
               "CPI_all")])


lm.elan3 = lm(ElantraSales ~ MonthFactor+Unemployment+CPI_energy+CPI_all,
              data=training)
summary(lm.elan3)

predTest = predict(lm.elan3, newdata=test)
SSE = sum((test$ElantraSales - predTest)^2)
mean(training$ElantraSales)
SST = sum((test$ElantraSales - mean(training$ElantraSales))^2)
R2 = 1 - SSE/SST
max(test$ElantraSales - predTest)
