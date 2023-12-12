Load the iris dataset
data("iris")
names(iris)
new_data<-subset(iris, select = c(-Species))
new_data
cl<-kmeans(new_data, 3)
cl
data<-new_data
wss<-sapply(1:15, function(k){kmeans(data, k)$tot.withinss})
wss
plot(1:15, wss, type='b', pch=19, frame = FALSE, xlab = "Number of clusters K", ylab ="Total within-sum of squares")
install.packages("cluster")
library(cluster)
clusplot(new_data, cl$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
cl$cluster
cl$centers
clusters<-hclust(dist(iris[, 3:4]))
plot(clusters)
clusterCut<-cutree(clusters, 3)
table(clusterCut, iris$Species)
install.packages("ggplot2")
library(ggplot2)
ggplot (iris, aes (Petal.Length, Petal.Width, color = Species)) + 
  geom_point (alpha = 0.4, size = 3.5) + geom_point (col = clusterCut) + 
  scale_color_manual (values = c("black", "red", "green"))
install.packages("cluster")
install.packages("factoextra")
# Load the required libraries
library(cluster)
library(factoextra)
library(datasets)
# Load the Iris dataset
data("iris")
iris_data <- iris[, 1:4]  # Selecting only the numeric columns
# Determine the optimal number of clusters using the elbow method
set.seed(123)
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(iris_data, centers = i, nstart = 10)
  wss[i] <- kmeans_model$tot.withinss
}
# Plot the elbow method graph
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within-cluster Sum of Squares")
# Determine the optimal number of clusters using silhouette coefficient
silhouette <- numeric(10)
for (i in 2:10) {
  kmeans_model <- kmeans(iris_data, centers = i, nstart = 10)
  silhouette[i] <- silhouette_avg(kmeans_model$cluster, dist(iris_data))
}
# Plot the silhouette coefficient graph
plot(2:10, silhouette[2:10], type = "b", xlab = "Number of Clusters",
     ylab = "Average Silhouette Width")
# Perform K-means clustering with the selected number of clusters
optimal_clusters <- 3  # You can choose the optimal number from the plots

