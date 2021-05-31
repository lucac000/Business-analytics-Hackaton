# import S&P500 dataset
install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/core/s-and-p-500-companies-financials/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}

# Optimizing dataset
data <- read.csv("C:/Users/Gianluca Nardo/Desktop/FINTECH/Courses/Business Analytics/Hackathon Project/constituents-financials_csv.csv")

# build the vector of company names 
# ?
company_symbol <- data[,c(1)]
company_names <- data[,c(2)]

# Create filtered (only numeric data)
# Deleting features 1,2,3
f.data <- data[, -c(1:3)] # delete columns 1 through 3

# standardize data
standardize <- function(x) {
  return ((x - mean(x)) / sd(x)) }
X.standardized <- as.data.frame(lapply(X, standardize))

sf.data <- as.data.frame(lapply(f.data, standardize))

# Compute size and dimensionality (original datset)
# Rows : 495
# Features : 13
dim(data)

# Perform PCA
pc.data <- princomp(sf.data, scores=T)
summary(pc.data)

# make a barplot of the variances of the  features
barplot(diag(var(sf.data)), las=2, main='Original features', ylim=c(0,8), ylab='Variances')

# make a barplot of the variances of the PCs (explained total variance)
barplot(pc.data$sdev^2, las=2, main='Principal components', ylim=c(0,8), ylab='Variances')

# make a barplot of the cumulated variances of the PCs (cumulated explained total variance)
barplot(cumsum(pc.data$sdev^2), las=2, main='Principal components', ylim=c(0,12), ylab='Variances')

# Compute the loadings
load.data    <- pc.data$loadings
load.data

# Make a barplot for each of the 8 loading vectors
par(mfcol = c(5,2))
for(i in 1:10)
{
  barplot(load.data[,i], ylim = c(-1, 1), main=paste('PC', i))
  abline(h=0)
}


# Compute the scores
scores.data <- pc.data$scores
scores.data

# Make boxplots of the features 
boxplot(sf.data, las=2, col='orange', main='Original features')

# Make boxplots of the scores of the PCs
boxplot(scores.data, las=2, col='orange', main='Principal components')
  

# Plot and interpret the first 4 PCs
par(mfrow=c(4,1))
for(i in 1:4)
{
  plot(cbind(scores.data[,i], 0), asp=1, pch=16, col='orange', 
       xlim=c(-4,4), main=paste('PC',i), ylab='', xlab='Scores')
  text(cbind(scores.data[,i], 0), labels = company, pos=3, srt=90, cex=0.75)
  abline(v=0, lty=2)
  arrows(0,0,load.data[,i]*sqrt(sum(diag(var(sf.data)))), 0, col='blue', pch=16, lwd=2)
  text(cbind(load.data[,i]*sqrt(sum(diag(var(sf.data)))), 0), labels = dimnames(sf.data)[[2]], col='blue', pos=1, srt=90)
}

# Install plotrix package
install.packages( "plotrix" )
library(plotrix)

# Plot and give an interpretation to the first 2 PCs jointly
plot(scores.data[,1],scores.data[,2],type="n",xlab="pc1",ylab="pc2", asp=1)
text(scores.data[,1],scores.data[,2],company, cex=0.5)
abline(h=0, lty=2)
abline(v=0, lty=2)
draw.circle(0,0,sqrt(sum(diag(var(sf.data)))), lty=2)
arrows(0,0,load.data[,1]*sqrt(sum(diag(var(sf.data)))), load.data[,2]*sqrt(sum(diag(var(sf.data)))), col='blue', pch=16, lwd=2)
text(load.data[,1:2]*sqrt(sum(diag(var(sf.data)))), labels = dimnames(sf.data)[[2]], col='blue')

# Plot and give an interpretation to the first 2 PCs jointly (ZOOMED)
plot(scores.data[,1],scores.data[,2],type="n",xlim=c(-3,3),ylim=c(-3,3), xlab="pc1",ylab="pc2", asp=1)
text(scores.data[,1],scores.data[,2],company, cex=0.5)
abline(h=0, lty=2)
abline(v=0, lty=2)
draw.circle(0,0,sqrt(sum(diag(var(sf.data)))), lty=2)
arrows(0,0,load.data[,1]*sqrt(sum(diag(var(sf.data)))), load.data[,2]*sqrt(sum(diag(var(sf.data)))), col='blue', pch=16, lwd=2)
text(load.data[,1:2]*sqrt(sum(diag(var(sf.data)))), labels = dimnames(sf.data)[[2]], col='blue')


# Project data points on the space spanned by the k-th PC

par(mfrow=c(2,5))
media <- colMeans(sf.data)
for(i in 1:10)
{
  projection <- matrix(media, dim(sf.data)[[1]], dim(sf.data)[[2]], byrow=T) + scores.data[,i] %*% t(load.data[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(sf.data))
  matplot(media, type='l', lwd=2, add=T)
}

# Project data points on the space spanned by the first k PCs

par(mfrow=c(2,5))
media <- colMeans(sf.data)
matplot(media, type='l', main = 'First 0 PCs', lwd=2, ylim=range(sf.data))
projection <- matrix(media, dim(sf.data)[[1]], dim(sf.data)[[2]], byrow=T)
for(i in 1:8)
{
  projection <- projection + scores.data[,i] %*% t(load.data[,i])
  matplot(t(projection), type='l', main = paste('First', i, 'PCs'), ylim=range(sf.data))
  matplot(media, type='l', lwd=2, add=T)
}