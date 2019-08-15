###############################
# Henry Qiu and Goktug Cengiz #
###############################

library(FactoMineR)
library(missForest)
library(mice)
library(fpc)
library(ggplot2)
library(gridExtra)

#Reading the data
DataRusset = read.delim("/Users/goktugcengiz/Desktop/Datasets/Russet_ineqdata2.txt", header = TRUE, sep="\t", dec =".")
#Data Understanding
str(DataRusset)
summary(DataRusset)
apply(DataRusset, 2, function(x) sum(is.na(x))) #Missing Value Detection
md.pattern(DataRusset) #Missing Value Visualization
#Data Preprocessing
#Missing Value Imputation with RandomForest
DR = missForest(DataRusset)
DR = DR$ximp
DR$ecks = round(DR$ecks)
DR$Rent = round(DR$Rent)
sum(is.na(DR))

#1.
#Perform the Principal Components Analysis. 
#Take the democracy index as supplementary variable, 
#whereas the remaining ones are active 
#and   CUBA   as   supplementary   individual 
res.pca = PCA(DR, quali.sup = 9, ind.sup = 11, scale.unit = TRUE, graph = TRUE)
print(res.pca)
eigenvalues = res.pca$eig

#2.
#Interpret the first two obtained factors.
summary(res.pca, ncp = 2)
dimdesc(res.pca, axes = 1:2) #correlation between each variable and the principal component of rank s is calculated. correlation coefficients are sorted and significant ones are output
head(eigenvalues[, 1:2]) 
plot(res.pca, cex = 0.8)
barplot(eigenvalues[,1],main="Eigenvalues",names.arg=1:nrow(eigenvalues))

#3.
#Decide the number of significant dimensions that you retain 
#(by subtracting the average eigenvalue
# and represent the new obtained eigenvalues in a new screeplot).
newEigenvalues = eigenvalues[ ,1] - mean(eigenvalues[,1])
barplot(newEigenvalues, names.arg=1:length(newEigenvalues), 
        main = "Scree Plot",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = seq(0.7, 9.5, 1.2), newEigenvalues,
      type="b", pch=19, col = "red")

sigDim = eigenvalues[ ,1][newEigenvalues > 0]
plot(sigDim, type="b", main="Eigenvalues")
numSigDim = length(sigDim)

#4.
#Perform a hierarchical clustering with the significant factors, 
#decide the number of final classes to obtain and 
#perform a consolidation operation of the clustering.
projections = res.pca$ind$coord[,1:numSigDim]
distMatrix = dist(projections)

hierClustSingl = hclust(distMatrix,method = "single")
plot(hierClustSingl, main= "Single Linkage")

hierClustComp = hclust(distMatrix,method = "complete")
plot(hierClustComp, main= "Complete Linkage")

hierClustAver = hclust(distMatrix,method = "average")
plot(hierClustAver, main= "Average Linkage")

hierClustCentr = hclust(distMatrix,method = "centroid")
plot(hierClustCentr, main= "Centroid Linkage")

hierClustMed = hclust(distMatrix,method = "median")
plot(hierClustMed, main= "Median Linkage")

hierClustWard = hclust(distMatrix,method = "ward.D2")
plot(hierClustWard, main= "Ward's Method")

#Choose Ward's Method
resCut2 <- cutree(hierClustWard, k = 2)
resCut3 <- cutree(hierClustWard, k = 3)
resCut4 <- cutree(hierClustWard, k = 4)
resCut5 <- cutree(hierClustWard, k = 5)
resCut6 <- cutree(hierClustWard, k = 6)
resCut7 <- cutree(hierClustWard, k = 7)
resCut8 <- cutree(hierClustWard, k = 8)
calinHara2 <- calinhara(projections,resCut2)
calinHara3 <- calinhara(projections,resCut3)
calinHara4 <- calinhara(projections,resCut4)
calinHara5 <- calinhara(projections,resCut5)
calinHara6 <- calinhara(projections,resCut6)
calinHara7 <- calinhara(projections,resCut7)
calinHara8 <- calinhara(projections,resCut8)

numClusters = 4

centroids <- NULL
for(k in 1:numClusters){
  prueba <- projections[resCut4 == k, , drop = FALSE]
  centroidPerCluster <- colMeans(prueba)
  centroids <- rbind(centroids,centroidPerCluster)
}
print(centroids)

kmeanRes <- kmeans(projections, centroids)
print("K-means Clustering after consolidation")
print(kmeanRes$cluster)

#5.
#Compute the Calinski-Harabassz index and
#compare before and after the consolidation step.
calinHaraBef <- calinhara(projections,resCut6)
calinHaraAft <- calinhara(projections,kmeanRes$cluster)

#6.
#Using the function catdes interpret and name the obtained clusters and
#represent them in the first factorial display.

demoRow <-DataRusset$demo
demoRow <- demoRow[-11:-11]#Removing the instance of the outlier Cuba
demoColumn  <- matrix(demoRow,ncol=1)

projectionsDataFrame = as.data.frame(cbind(projections, demoColumn))
projectionsDataFrame$V4 <- as.factor(projectionsDataFrame$V4)
catdes(projectionsDataFrame,num.var=4)

plot(x = projections[,1], y = projections[,2])

labls = row.names(projections)
text(as.data.frame(projections),labels = labls)

g <- ggplot(as.data.frame(projections), aes(x=projections[,1], y=projections[,2],
            color = as.factor(kmeanRes$cluster))) +  geom_point() +
            labs(x = '1st Component', y = '2nd Component') + geom_text(aes(label=labls))
grid.arrange(g, ncol = 1)



