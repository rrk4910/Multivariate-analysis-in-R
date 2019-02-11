# Cluster analysis in R
# Installing and loading required R packages for cluster analysis

# Install package "cluster" for computing clustering
install.packages("cluster")
# Install package "factoextra" for elegant ggplot2-based data visualization
install.packages("factoextra")

#Loading required packages
library("cluster")
library("factoextra")

# Importing the data from .CSV file
mydata <- read.csv(file.choose(), header = TRUE)

# To display head of the data set
head(mydata, 5) # Print the first 5 rows

#To remove any missing value that might be present in the data:
mydata <- na.omit(mydata)

# Scaling/standardizing the data
mydata_scaled = scale(mydata)
head(mydata_scaled, n = 5) # Print the first 5 rows

# To compute Euclidean distance or one of: "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
dist.euclidean <- dist(mydata_scaled, method = "euclidean")

# Reformat as a matrix of Euclidean distance
# Subset the first 10 columns and rows and Round the values
round(as.matrix(dist.euclidean)[1:10, 1:10], 1)

#To compute and visualize distance matrix using the functions get_dist() and fviz_dist() in factoextra R package
#get_dist(): for computing a distance matrix between the rows of a data matrix. Compared to the standard dist() function, it
#supports correlation-based distance measures including "pearson", "kendall" and "spearman" methods

# Computing correlation based distances
dist.correlation <- get_dist(mydata_scaled, method = "pearson")

# Display a subset
round(as.matrix(dist.correlation)[1:10, 1:10], 1)

# Visualizing Euclidean distance matrices
fviz_dist(dist.euclidean)

# Visualizing correlation distance matrices
fviz_dist(dist.correlation)

# Computing k-means clustering

# Estimating the optimal number of clusters
#fviz_nbclust(mydata_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
km.res <- kmeans(mydata_scaled, 4, nstart = 25)
print(km.res)
# To compute the mean of each variables by clusters using the original data
aggregate(mydata, by=list(cluster=km.res$cluster), mean)

# To add the point classifications to the original data
dd <- cbind(mydata, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster
head(km.res$cluster, 4)
# Cluster size
km.res$size
# Cluster means
km.res$centers
# Total sum of squares (TSS)
km.res$totss
# Vector of within-cluster sum of squares, one component per cluster
km.res$withinss

# Visualizing k-means clusters
fviz_cluster(km.res, data = mydata_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)


# Hierarchical Clustering

# Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(mydata_scaled, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]

# Linkage
res.hc <- hclust(d = res.dist, method = "ward.D2")

# Dendrogram
fviz_dend(res.hc, cex = 0.5)

# Compute cophentic distance
res.coph <- cophenetic(res.hc) # Verify the cluster tree

# Correlation between cophenetic distance and the original distance
cor(res.dist, res.coph)
res.hc2 <- hclust(res.dist, method = "average")
cor(res.dist, cophenetic(res.hc2))

# Cut the dendrogram into different groups
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)
# Number of members in each cluster
table(grp)
# Get the names for the members of cluster 1
rownames(df)[grp == 1]
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
fviz_cluster(list(data = mydata_scaled, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Cluster R package
library("cluster")
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = mydata, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
fviz_dend(res.agnes, cex = 0.6, k = 4)

# DIvisive ANAlysis Clustering
res.diana <- diana(x = mydata, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)
fviz_dend(res.diana, cex = 0.6, k = 4)



# Fuzzy Clustering
df <- scale(mydata_scaled) # Standardize the data
res.fanny <- fanny(df, 2) # Compute fuzzy clustering with k = 2
head(res.fanny$membership, 3) # Membership coefficients
res.fanny$coeff # Dunn's partition coefficient
head(res.fanny$clustering) # Observation groups
#To visualize observation groups
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

# To evaluate the goodnesss of the clustering results, plot the silhouette coefficient
fviz_silhouette(res.fanny, palette = "jco",
                ggtheme = theme_minimal())

