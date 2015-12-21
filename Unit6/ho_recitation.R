# flower
flower = read.csv("flower.csv", header=FALSE)
str(flower)
flowerVec = as.vector(as.matrix(flower))
str(flowerVec)

distFlowerVec = dist(flowerVec, method="euclidean")
hcFlower = hclust(distFlowerVec, method="ward.D2")
plot(hcFlower)
rect.hclust(hcFlower, k=3)
flowerClust = cutree(hcFlower, k= 3)
flowerClust
tapply(flowerVec, flowerClust, mean)
image(matrix(data=flowerClust, nrow=50, ncol=50), axes=FALSE, col=grey(seq(0,1,length=3)))

# healthy 
healthy = read.csv("healthy.csv", header = FALSE)
str(healthy)
healthyMat = as.matrix(healthy)
str(healthyMat)
image(healthyMat, axes=FALSE, col=grey(seq(0,1,length=256)))
healthyVec = as.vector(healthyMat)
str(healthyVec)
#distHealthy = dist(healthyVec)
# 각 pixel 간의 거리를 구하기에는 너무 멀다. 

# kmeans 
k = 5
dim(healthyMat)
km = kmeans(healthyVec, centers = k, iter.max = 1000)
km$cluster
dim(healthyMat)
resultMat = km$cluster
dim(resultMat) = dim(healthyMat)
image(resultMat, axes=FALSE, col=grey(seq(0,1,length=5)))
km$withinss

# cluster 개수 정하기
numClusters = seq(2,10,1)
sumWithinss = sapply(numClusters, function(x) {sum(kmeans(healthyVec, centers=x, iter.max=1000)$withinss)})
plot(numClusters, sumWithinss, type="o")

# 이 결과를 적용 
tumor = read.csv("tumor.csv", header=FALSE)
str(tumor)
tumorVect = as.vector(as.matrix(tumor))
str(tumorVect)
library(flexclust)
tumorKCCA = as.kcca(km, healthyVec)
tumorPred = predict(tumorKCCA, newdata=tumorVect)
summary(tumorPred)
unique(tumorPred)
tumorMat = tumorPred
dim(tumorMat) = dim(as.matrix(tumor))
image(tumorMat, axes=FALSE, col=grey(seq(0,1,length=5)))
image(as.matrix(tumor), axes=FALSE, col=grey(seq(0,1,length=256)))
