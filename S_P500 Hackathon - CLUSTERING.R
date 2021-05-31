# Clustering first part
# S&P500

# Data import S&P500 cleaned
constituents <- read.csv("C:/Users/Gianluca Nardo/Desktop/FINTECH/Courses/Business Analytics/Hackathon Project/constituents-financials_csv.csv")

# Bulding sub data sets
library(tidyverse)
data.symbol <- constituents[,1]
data.name <- constituents[,2]
data.sector <- constituents[,3]
data.features <- constituents[,4:13]
data.3features <- data.features %>% select(1,2,7)

library(resample)
library(rgl)

# Computing the Euclidean distance between the data points
dist(data.features)

# Heatmap of the distance matrix
image(as.matrix(dist(data.features)), x=1:dim(data.features)[1], y=1:dim(data.features)[1])
max(dist(data.features))

### Means and Variances ###

# Compute the vectors of means
colMeans(data.features)

# Compute the vectors of variance
colVars(data.features)

# Compute the total variance
sum(colVars(data.features))

# Compute the vectors of means
#colMeans(data.features.social)

# Compute the vectors of variance
#colVars(data.features.social)

# Compute the total variance
#sum(colVars(data.features.social))

### K-mean Clustering ###

### K-means height and weight ###
plot(scores.data[,1:2], pch=16, asp=1)

clustK <- kmeans(scores.data[,1:2], 3)

plot(scores.data[,1:2], col=clustK$cluster, asp = 1, pch=16) 
clustK$iter
clustK$cluster
clustK$size
clustK$centers
clustK$tot.withinss/dim(scores.data[,1:2])[1]
clustK$totss/dim(scores.data[,1:2])[1]

points(rbind(colMeans(scores.data[,1:2])), pch=17, col='orange', cex=2) 
points(clustK$centers, pch=17, col=1:3, cex=2) 

# Comparison

table(clustK$cluster, data.sector)

# Stability Check

within.SS <- matrix(NA, 1000, 10)
for(rep in 1:1000)
{
  for(k in 1:10)
  {
    clustK <- kmeans(data.features, k)
    within.SS[rep,k] <- clustK$tot.withinss
  }
}

boxplot(within.SS/dim(data.features)[1], main='Within SS')

# NB: cluster instability 
# NB: label matching
# NB: variable standardization or rescaling
# NB: K-medoids

### Hierarchical Agglomerative Clustering ###

Q <-data.features[,3:2]
# Q <-data.features.social

# Computing the matrix of distances
par(mfrow=c(1,1))
d <- dist(Q)
image(as.matrix(d), x=1:dim(data.features)[1], y=1:dim(data.features)[1])

par(mfrow=c(1,2))

# Average linkage
clusta <- hclust(d, method='average')
plot(clusta, main='average', xlab='', sub='')

# Ward linkage
clustw <- hclust(d, method='ward.D')
plot(clustw, main='ward', xlab='', sub='')

# More info
clusta$merge
clusta$height
clusta$order

# extracting the clusters
clustera <- cutree(hclust(d, method='average'), 4)
plot(data.features, col=clustera+1, pch=16, asp=1)

clusterw <- cutree(hclust(d, method='ward.D'), 3)
plot(data.features, col=clusterw+1, pch=16, asp=1)

