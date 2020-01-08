##Clustering Analysis - BEGIN##


clust <- function(data,vars){
  
  res.clus <- list()
  
  ### Euclidean distance matrix
  # Filtering some data from 2015
  newdata<-na.omit(data[,c(vars)])
  # Distance matrix
  d <- dist(as.matrix(newdata, method="euclidean") )
  #res.clus$distance <- d
  
  ### Hierarchical clustering
  # Single-linkage clustering (method= single)
  hc.sing <- hclust(d, method = "single")
  res.clus$HierarchicalClusteringSingle <- hc.sing
  
  
  # Complete-linkage clustering (method= complete by default)
  hc.comp <- hclust(d)
  res.clus$HierarchicalClusteringComplete <- hc.comp
  
  ### Clustering with k-means
  # K-means cluster analysis
  options(digits=3)
  set.seed(20000)
  fit <- kmeansruns(newdata,krange=1:ncol(newdata),critout=TRUE,runs=2,criterion="ch")
  best.k <<- fit$bestk 
  res.clus$KMeans <- fit
  res.clus$bestk <- best.k
  res.clus$KMeans.bestk <- kmeans(x=newdata,centers = best.k)
  # Append cluster assignment
  newdata <- data.frame(newdata, fit$cluster)
  res.clus$fit <- newdata
  res.clus$data <- newdata
  res.clus
}


##Clustering Analysis - END##