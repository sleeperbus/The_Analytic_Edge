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
distHealthy = dist(healthyVec)
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



