FluTrain = read.csv("FluTrain.csv")
hist(FluTrain$ILI)
hist(log(FluTrain$ILI))
plot(log(FluTrain$ILI), FluTrain$Queries)
FluTrend1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries)
FluTest = read.csv("FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]
(PredTest1 - FluTest$ILI)/FluTest$ILI

SSE = sum((PredTest1-FluTest$ILI)^2)
RMSE_train = sqrt(mean((PredTest1 - FluTest$ILI)^2))

library("zoo")
ILILAG2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)    
FluTrain$ILILAG2 = coredata(ILILAG2)
hist(log(FluTrain$ILILAG2))
plot(log(FluTrain$ILI) ~ log(FluTrain$ILILAG2), xlab="lag 2 weeks")

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILAG2), data=FluTrain)
summary(FluTrend2)

ILILAG2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)    
FluTest$ILILAG2 = coredata(ILILAG2)
FluTest$ILILAG2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILAG2[2] = FluTrain$ILI[nrow(FluTrain)]
head(FluTest$ILILAG2)
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
RMSE_test = sqrt(mean((PredTest2 - FluTest$ILI)^2))
