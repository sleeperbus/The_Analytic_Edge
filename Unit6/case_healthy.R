healthy = read.csv("healthy.csv", header=F)
str(healthy)
healtyMatrix = as.matrix(healthy)
image(healtyMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

healthyVector = as.vector(healtyMatrix)
distances = dist(healthyVector, method="euclidean")
x = length(healthyVector)
x*(x-1)/2

k = 5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

groups = KMC$cluster
dim(groups) = c(nrow(healthy), ncol(healthy))
image(groups, axes=FALSE, col=rainbow(k))
numCluster = seq(2,10,1)
sumWithinss = sapply(2:10, 
                     function(x) {
                         sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss)
                     }
            )
plot(2:10, sumWithinss, type="b")
                    
