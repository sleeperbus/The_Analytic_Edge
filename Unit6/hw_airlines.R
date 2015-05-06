airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)
head(airlines)

# normalize data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
str(airlinesNorm)
summary(airlinesNorm)

# hclust
distances = dist(airlinesNorm, method="euclidean")
clustH = hclust(distances, method="ward.D")
plot(clustH)
rect.hclust(clustH, k=5)
groupsH = cutree(clustH, k=5)
table(groupsH)

spl = split(airlines, groupsH)
lapply(spl, colMeans)


# kmeans
set.seed(88)
kmc = kmeans(airlines, centers=5, iter.max=1000)
table(kmc$cluster)
spl2 = split(airlines, kmc$cluster)
lapply(spl2, colMeans)
table(groupsH, kmc$cluster)
