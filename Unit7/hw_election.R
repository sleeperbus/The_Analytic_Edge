library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
    geom_polygon(fill = "white", color = "black")

polling = read.csv("PollingImputed.csv")
str(polling)
Train = subset(polling, Year >= 2004 & Year <= 2008)
Test= subset(polling, Year == 2012)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data=Train, family=binomial)
summary(mod2)
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
summary(predictionDataFrame)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by="region")
predictionMap = predictionMap[order(predictionMap$group, predictionMap$order),]

ggplot(predictionMap, aes(x=long, y=lat, group=group, 
                          fill=TestPredictionBinary, alpha=0.5)) + 
    geom_polygon(color="black") +
    scale_fill_gradient(guide="legend", low="blue", high="red", breaks=c(0,1),
                        labels=c("Democrat", "Republican"))

ggplot(predictionMap, aes(x=long, y=lat, group=group, 
                          fill=TestPrediction)) + 
    geom_polygon(color="black") +
    scale_fill_gradient(guide="legend", low="blue", high="red")

