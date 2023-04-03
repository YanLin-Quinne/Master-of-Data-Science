#DEVUL assignment2: writed by Yan Lin

library(factoextra)
library(FactoMineR)
library(tidyverse)
library(ggplot2)
library( cluster)

#loading the data
d1 <- read.csv("~/Desktop/wholesale.csv")
d1

#overview of data
library(skimr)
skim(d1)
names(d1)
str(d1)

#check missing values
colSums(is.na(d1))

#Descriptive statistics 
desc_stats <- data.frame(
  Min = apply(d1, 2, min), # minimum
  Q1 = apply(d1, 2, function(x) quantile(x, 0.25)), # 1st quartile (Q1)
  Med = apply(d1, 2, median), # median
  Mean = apply(d1, 2, mean), # mean
  Q3 = apply(d1, 2, function(x) quantile(x, 0.75)), # 3rd quartile (Q3)
  Max = apply(d1, 2, max), # Maximum
  SD = apply(d1, 2, sd) # Standard deviation
)
desc_stats <- round(desc_stats, 2)
head(desc_stats)
#Descriptive statistics is similar to summary function
summary(d1)

#Create a histogram for each column
df_num <- df[sapply(d1, is.numeric)]
par(mfrow = c(ceiling(ncol(df_num)/3), 3))  # Set up a grid of plots
for (i in 1:ncol(df_num)) {
  hist(df_num[,i], main = names(df_num)[i], xlab = "")
}
dev.off()

# Calculate the correlation matrix
cor_matrix <- cor(d1)
print(cor_matrix)

# Calculate the covariance matrix
cov_matrix <- cov(d1)
print(cov_matrix)

# Visualize relationships between variables using scatterplot matrix
library(car)
scatterplotMatrix(d1, col=4,
                  pch=16, smooth=FALSE, regLine=FALSE,
                  diagonal=list(method ="histogram"),
                  main="Scatterplot Matrix",
                  cex.main=0.8)
library(GGally)
ggpairs(d1)
library(corrplot)
cor_matrix <- cor(d1)
corrplot(cor_matrix, method = "circle",diag=FALSE)
corrplot(cor_matrix, method = "number",diag=FALSE)
library(ggcorrplot)
ggcorrplot(cor_matrix, hc.order = TRUE,type = "upper",
           outline.color = "white", colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE, lab_size = 3, lab_col = "black") +
  ggtitle("Correlation Matrix") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
library(psych)
pairs.panels(d1,method='pearson',
             hist.col='#00AFBB',
             density=TRUE,
             ellipses=TRUE)
#milk, grocery, detergents_paper These three variables are highly correlated

# Boxplots to visualize distribution and detect outliers
boxplot(d1,col=2:7,main='Boxplot of wholesale dataset')

#Find the outliers and exxtremes
find_outliers_extremes <- function(d1, k1 = 1.5, k2 = 3) {
  # initialize an empty list to store results for each column
  results <- list()
  # loop through each column in the data
  for (i in seq_along(d1)) {
    col <- d1[[i]]
    # calculate quartiles and interquartile range
    q1 <- quantile(col, 0.25, na.rm = TRUE)
    q3 <- quantile(col, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    # calculate lower and upper bounds for outliers
    lower_bound <- q1 - k1 * iqr
    upper_bound <- q3 + k1 * iqr
    # calculate lower and upper bounds for extremes
    lower_fence <- q1 - k2 * iqr
    upper_fence <- q3 + k2 * iqr
    # identify outliers and extremes
    outliers <- which(col < lower_bound | col > upper_bound)
    extremes <- which(col < lower_fence | col > upper_fence)
    # add results for this column to the list
    results[[i]] <- list(outliers = outliers, extremes = extremes)
  }
  return(results)
}
outliers_extremes <- find_outliers_extremes(d1)
df_with_NA <- d1
for (i in 1:length(outliers_extremes)) {
  df_with_NA[outliers_extremes[[i]]$outliers, i] <- NA
  df_with_NA[outliers_extremes[[i]]$extremes, i] <- NA
}
colSums(is.na(df_with_NA)) #48
library(mice)
imputed_data <- mice(df_with_NA, m = 5, maxit = 50, seed = 1)
d2 <- complete(imputed_data)
colSums(is.na(d2))

#Compare two boxplots
par(mfrow=c(2,1))
boxplot(d1,col=2:7,main='Boxplot of wholesale dataset')
boxplot(d2,col=2:7,main='Boxplot of wholesale cleaned dataset')
dev.off()


#Testing the data set for normal distribution
normality <- data.frame(p_value = numeric(0), test_statistic = numeric(0))
for (i in colnames(d2)) {
  # Perform the Shapiro-Wilk test for normality
  shapiro_test <- shapiro.test(d2[[i]])
  # Store the p-value and test statistic in the 'normality' data frame
  normality <- rbind(normality, c(shapiro_test$p.value, shapiro_test$statistic))
}
rownames(normality) <- colnames(d2)
colnames(normality) <- c('p-value','test-statistic')
normality  
round(normality,2)
#Only Milk and Frozen seem to be normally distributed

num_columns <- 6
plot_rows <- ceiling(sqrt(num_columns))
plot_cols <- ceiling(num_columns / plot_rows)
layout(matrix(1:(plot_rows * plot_cols), nrow = plot_rows, ncol = plot_cols, byrow = TRUE))
for (i in 1:ncol(d2)) {
  # Create a QQ plot for each variable
  qqnorm(d2[, i], main = colnames(d2)[i])
  qqline(d2[, i], col = "red")
}
dev.off()

#Standardised the dataset
apply(d2, 2, sd)
apply(d2, 2, var)
apply(d2, 2, mean) #Needs to be standardised
#They are vastly different, so there is need for scaling.

df.scaled <- scale(d2) #crucial pre-processing step
#Not normally distributed so need to scale



#PCA: The singular value decomposition in the principal component analysisis performed below
#while prcomp() uses singular value decomposition (svd), which can be more stable in some cases.

pr.out<-prcomp(d2,scale=TRUE)
summary(pr.out)

pr.out$center #Means and standard deviations of variables before standardisation.
pr.out$scale #Means and standard deviations of variables after standardisation.

#eigenvalues
pr.out$sdev^2
#PC loadings  vector
pr.out$rotation
#PC score
pr.out$x

summary(pr.out)
head(pr.out$x) #6 PC score vectors

#The Standard deviation of the principal components is the square root of the corresponding eigenvalues.
sd_all <- summary(pr.out)$importance[1,] #Standard deviation
sd_all^2/sum(sd_all^2) #pr.out$sdev

#scree plot
library(factoextra)
fviz_screeplot(pr.out, addlabels = TRUE) 
#The rule of thumb recommends retaining the component that explains 80-90% of the variability in the original data set

pr.out$sdev #Standard deviation of each principal component
pr.var=pr.out$sdev^2
pr.var  #Variance explained by each principal component
pve=pr.var/sum(pr.var)
pve 
par(mfrow=c(1,2))
plot(pve,xlab='Principal Component',ylab='Proportion of Variance Explained',type='b',
     main='Proportion of variance explained')
plot(cumsum(pve),xlab='Principal Component',
     ylab='Cumulative Proportion of Variance Explained',type='b',
     main='Cummulative proportion of variance explained')
dev.off()
plot(summary(pr.out)$importance[2,], 
     type="b", xlab="PCs", ylab="Variability explained")
#The summary shows that 3 components are enough to keep 80% of the variability in data. 
#Also choosing 3 principal components seems reasonable based on the so-called elbow method.


#biplot
fviz_pca_biplot(pr.out,addlabels = TRUE)
library(rgl)
scores <- pr.out$x[, 1:3]
loadings <- pr.out$rotation[, 1:3]
plot3d(scores[,1:3])
#text3d(scores[,1:3],texts=rownames(d2),cex=0.8)
text3d(pr.out$rotation[,1:3], 
       texts=rownames(pr.out$rotation), cex=0.8,
       col="red")
coords <- NULL
for (i in 1:nrow(pr.out$rotation)) {
  coords <- rbind(c(0, 0, 0), pr.out$rotation[i, 1:3])
  lines3d(coords, col = sample(colors(), 1), lwd = 4)
}
normal_vector <- colMeans(pr.out$rotation[, 1:3])
plane_intercept <- -sum(normal_vector * c(0, 0, 0))
planes3d(normal_vector[1], normal_vector[2], normal_vector[3], plane_intercept, alpha = 0.5, col = "blue")

#Correlation between variables and PCs
var <- get_pca_var(pr.out)
var$cor  #Correlation between variables and PC
cor(d2,pr.out$x[,1:3]) #Variable Correlation Chart
fviz_pca_var(pr.out) 
fviz_pca_var(pr.out, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Cos2 between variables and PCs
var$cos2 #quality of representation (var$cor)^2
cos2 <- (cor(d2, pr.out$x[,1:3]))^2
cos2
fviz_cos2(pr.out,choice='var',axes=1) 
fviz_cos2(pr.out,choice='var',axes=2)
fviz_cos2(pr.out,choice='var',axes=3)

##Contribution between variables and PCs
var$contrib 
#any variable whose height is up to 16.66 and 
#above is considered to have contributed significantly to the component.
fviz_contrib(pr.out, choice = "var", axes = 1, top = 10) #Variable contribution to PC1
fviz_contrib(pr.out, choice = "var", axes = 2, top = 10) #Variable contribution to PC2
fviz_contrib(pr.out, choice = "var", axes = 3, top = 10) #Variable contribution to PC3


#Use the characteristic decomposition method
#Check by evaluating the standard deviation of the variables
sd<-apply(d2,2,sd)
sd
#Variables should be scaled/use correlation matrix
S<-cor(d2)
eigdec<-eigen(S)
eigdec  #The values and vectors from the decomposition are the variances and the principal component loadings respectively.
eig <- eigdec$values #The value of the decomposition represents the variance
eig
sum(eig)

pc_loading <- eigdec$vectors
rownames(pc_loading) <- colnames(d2)
pc_loading #Eigenvectors

#The proportion of variance explained is as follows
eig<-eigdec$values
variance<-eig*100/sum(eig)
#Cumulative explained variance
cumvar<-cumsum(variance)
eig2<-data.frame(eig=eig,variance=variance,cumvariance=cumvar)
eig2

#scree-plot
barplot(eig2[,2],names.arg=1:nrow(eig2),main='Scree Plot',
        xlab='Dimensions',ylab='Percentage of variances',col='steelblue')
lines(x = 1:nrow(eig2), eig2[, 2],
      type="b", pch=19, col = "red")

pc_score <- as.matrix(scale(d2))%*% pc_loading
colnames(pc_score) <- paste0("PC", 1:6)
pc_score[1:6,]

library(psych)
pairs.panels(pc_score,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
#The correlation between PCs (new coordinates) is essentially zero, as required
pc_score2 <- as.data.frame(pc_score[,1:3])
rownames(pc_score2) <- seq(1:440)
max_abs_index <- function(x) {
  return(which.max(abs(x)))
}
pc_score2$max_abs_col <- apply(pc_score2, 1, max_abs_index)
color_labels<- c('red','yellow','blue')
pc_score2$color <- color_labels[factor(pc_score2$max_abs_col, levels = 1:length(color_labels))]
pc_score2

cor(d2, pc_score[,1:3])
t(t(pc_loading)*sqrt(eig))

cos2 <- (cor(d2, pc_score[,1:3]))^2
cos2
comp.cos2 <- apply(cos2, 2, sum)
comp.cos2 # same as the corresponding eigenvalues

contrib2 <- function(cos2, comp.cos2){cos2*100/comp.cos2}
contrib <- t(apply(cos2,1, contrib2, comp.cos2))
contrib

names1 = c("Grocery", "Milk","Detergents_Paper","Delicassen", "Frozen","Fresh")
barplot(contrib[order(contrib[, 1],decreasing = T), 1], names.arg=names1, 
        main = "Contribution of variables to PC1",ylim=c(0,35),
        xlab = " ",
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=100/6, col="red",lty=3, lwd =1)
#fviz_contrib(pr.out, choice = "var", axes = 1, top = 10)

names2 =c("Frozen","Fresh","Delicassen","Detergents_Paper","Milk","Grocery")
barplot(contrib[order(contrib[, 2],decreasing = T), 2], names.arg=names2, 
        main = "Contribution of variables to PC2",
        xlab = " ",ylim=c(0,40),
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=100/6, col="red",lty=3, lwd =1)
#fviz_contrib(pr.out, choice = "var", axes = 2, top = 10)

names3 =c("Fresh","Delicassen","Detergents_Paper","Grocery","Frozen","Milk")
barplot(contrib[order(contrib[, 3],decreasing = T), 3], names.arg=names3, 
        main = "Contribution of variables to PC3",
        xlab = " ",ylim=c(0,55),
        ylab = "Percentage of variances",
        col ="steelblue",las=2,cex.names=0.7)
abline(h=100/6, col="red",lty=3, lwd =1)
#fviz_contrib(pr.out, choice = "var", axes = 3, top = 10)

#Principal component regression
df.pca <- prcomp(d2[,-3], scale=TRUE)
df.pca$x
Z=df.pca$x[,1:3]
df.lm<-lm(d2$Grocery~Z)
df.lm
df.pca$rotation[,1:3]%*%matrix(coef(df.lm)[-1], ncol=1)
library(pls)
data.pcr<-pcr(Grocery~Delicassen+Milk+Fresh+Frozen+Detergents_Paper,
              3,scale=TRUE,data=d2)
coef(data.pcr)



#Cluster analysis
#1.k-means clustering
df.scaled <- scale(d2)
#Choose the best k in the k-means algorithm, using the sum of the sums of squares within the clusters
fviz_nbclust(df.scaled, kmeans, method = "wss")+
  geom_vline( xintercept  =  3 , linetype  =  2 )
#total within-cluster sum of square (wss)
fviz_nbclust(df.scaled,kmeans, method = "silhouette")+
  labs(title = "K-means")

#This Figure is treated the same way as we did with the scree plot. 
#We locate the bend (knee) in the plot. 
#This seems to be at k=2. So we will use two clusters for the analysis.
# Compute k-means with k = 2
set.seed(123)
k2 <- kmeans(df.scaled, 2, nstart = 25)
names(k2)
#Try 25 different random initial cluster assignments and choose the best result corresponding to the one with the least variation within the clusters
#between_SS / total_SS =  31.2%，Percentage of variance explained by the mean of the clusters
k2$centers #2 centroids under 6 variables (6 dimensions)
k2$iter #1 iteration only
k2$size #252 188
k2$withinss #1021.4575  789.5534
k2$tot.withinss #1811.011
k2$totss #2634
k2$betweenss #822.9891
#Percentage of variance explained by the cluster means = (between_ss/total_ss)*100
# k2$betweenss=k2$totss -k2$tot.withinss
# 31.2 %= (k2$totss-k2$tot.withinss)/k2$totss= k2$betweenss/k2$totss

cols=c('red','darkgreen')
plot(df.scaled,col=cols[k2$cluster],main='K-means clustering with 2 clusters',xlab='',ylab='')
points(k2$centers,pch=19,cex=2,col=cols) #k=2 is not good enough, there is overlap

#Find the mean of 6 variables based on the newly created cluster = k2$centers #2 centroids under 6 variables (6 dimensions)
aggregate(scale(d2), by=list(cluster=k2$cluster), mean)

# Visualise kmeans clustering
fviz_cluster(k2, df.scaled, ellipse.type = "norm")
#It is clear from this result that the clusters are not well separated anymore.
#In fact, we generated overlapping clusters.

#We can visualise the clustering results using principal components 
#if the number of variable are greater than 2.

# Visualize the k-means clustering results using PCA
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$cluster <- k2$cluster
pca_cluster_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Results using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()
print(pca_cluster_plot)
#Looking at the results of k-means on the first 2 principal components is very intuitive

k3 <- kmeans(df.scaled, 3, nstart = 25)
k3 #between_SS / total_SS =  48.7 %
k3$tot.withinss #1531.347
k3$size
cols=c('red','darkgreen','blue')
plot(df.scaled,col=cols[k3$cluster],main='K-means clustering with 3 clusters',xlab='',ylab='')
points(k3$centers,pch=19,cex=2,col=cols) 

fviz_cluster(k3, d2, ellipse.type = "norm")
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$cluster <- k3$cluster
pca_cluster_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering Results using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()
print(pca_cluster_plot)


#2.K-medoids clustering
#The main advantage of K-medoids over K-means clustering is that it is less sensitive to outliers
#The most common method for clustering K-medoids is the PAM algorithm (Partitioning Around Medoids)
pam.res <- pam(df.scaled, k=2)
names(pam.res)
summary(pam.res)
pam.res$medoids # Medoids
pam.res$id.med #245 247
df.scaled[c(245,247),] #==pam.res$medoids Find the central point
pam.res$clustering  # Cluster vectors
#Visualization Cluster
fviz_cluster(pam.res, d2, ellipse.type = "norm")
clusplot( pam.res , main  =  " Cluster plot with K-medoids (pam), K = 2 " , color  =  TRUE )
#Silhouette
pam.res$silinfo
pam.res$silinfo$clus.avg.widths #Average silhouette width
pam.res$silinfo$avg.width #Overall average silhouette
pam.res$clusinfo #cluster1:193, cluster2:247
fviz_silhouette(silhouette(pam.res))
fviz_silhouette(silhouette(pam.res$clustering,dist(df.scaled)),palette='jco',ggtheme=theme_classic())
#there are a small number of si having a negative sign in cluster 1&2
# Compute silhouette
sil <- silhouette(pam.res)[, 1:3]
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#CLARA: Clustering Large Applications
clara.res <- clara(df.scaled, k=2)
names(clara.res)
clara.res
summary(clara.res)
clara.res$medoids #Centroids under 6 variables in 6 dimensions
clara.res$i.med  #i.med gives the IDs of the 2 observations used as centres (remember we are not using centroids here)
#75 375
df.scaled[c(75,375),] #==clara.res$medoids
clara.res$clustering
fviz_cluster(clara.res,d2,ellipse.type = 'norm')
clusplot( clara.res , 
          main  =  " Cluster plot with K-medoids (clara), K = 2 " , 
          color  =  TRUE )
clara.res$silinfo
clara.res$silinfo$clus.avg.widths #Average silhouette width
clara.res$silinfo$avg.width #Overall average silhouette
clara.res$clusinfo  #cluster1:214,cluster2:226
##pam is better than clara, k-medoids use pam later

# Visualising k-means and k-medoids clustering results using PCA
# Adding k-means and k-medoids clustering assignments to PCA scores
pca_scores <- as.data.frame(pr.out$x[, 1:2])
pca_scores$kmeans_cluster <- k2$cluster
pca_scores$kmedoids_cluster <- pam.res$clustering
pca_cluster_plot <- ggplot() +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = as.factor(kmeans_cluster)), alpha = 0.5) +
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = as.factor(kmedoids_cluster)), shape = 1, alpha = 0.5) +
  labs(title = "K-means and K-medoids Clustering Results using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()
print(pca_cluster_plot)

#Reference workshop3
par(mfrow=c(2,1))
kmeans_centers <- k2$centers
kmedoids_centers <- pam.res$medoids

plot(df.scaled, col = cols[k2$cluster], main = 'K-means and K-medoids clustering with 2 clusters')
points(kmeans_centers, pch = 19, cex = 2, col = cols)
legend("topright", legend = c("K-means centers"), col = c("black"), pch =19, 
       pt.bg =NA, bty = "n")

plot(df.scaled, col = cols[pam.res$clustering], main = 'K-means and K-medoids clustering with 2 clusters')
points(kmedoids_centers, pch = 23, cex = 2, col = cols, bg = 'white')
legend("topright", legend = c("K-medoids centers"), col = c("black"), 
       pch = 23, pt.bg = c("white"), bty = "n")
dev.off()



library(cluster)
# Compute the Silhouette Score
kmeans_silhouette_score <- mean(silhouette(k2$cluster, dist(df.scaled))[, "sil_width"])
kmeans_silhouette_score  #0.2849978
kmeans3_silhouette_score <- mean(silhouette(k3$cluster, dist(df.scaled))[, "sil_width"])
kmeans3_silhouette_score  #0.2187303
kmedoids_silhouette_score <- mean(silhouette(pam.res$clustering, dist(df.scaled))[, "sil_width"])
kmedoids_silhouette_score #0.2782588
#The Silhouette Score ranges from -1 to 1. The closer the value to 1, the better the clustering.
#These two scores indicate that the quality of the clustering is fair, but there may be room for improvement.
#mean(silhouette(clara.res$clustering, dist(df.scaled))[, "sil_width"])
#[1] 0.2741116


#Evaluation of k-means and k-medoids: to use contour silhouette analysis
#compare:K-means
sil_kmeans<-silhouette(k2$cluster,dist(df.scaled))
head(sil_kmeans[,1:3],4)
k2$cluster[1:4]
sil_kmeans.sum<-summary(sil_kmeans)
sil_kmeans.sum$clus.avg.widths #Average silhouette width
sil_kmeans.sum$avg.width #Overall average silhouette
sil_kmeans.sum$clus.sizes ##Size of each cluster
fviz_silhouette(sil_kmeans,palette='jco',ggtheme=theme_classic()) #k-means
#there are a small number of si having a negative sign in cluster 2

sil_kmeans<-silhouette(k2$cluster,dist(df.scaled))
head(sil_kmeans[,1:3],4)
k2$cluster[1:4]
sil_kmeans.sum<-summary(sil_kmeans)
sil_kmeans.sum$clus.avg.widths #Average silhouette width
sil_kmeans.sum$avg.width #Overall average silhouette
sil_kmeans.sum$clus.sizes ##Size of each cluster
fviz_silhouette(sil_kmeans,palette='jco',ggtheme=theme_classic()) #k-means
#there are a small number of si having a negative sign in cluster 2

#compare:3-clustering 
sil_kmeans3<-silhouette(k3$cluster,dist(df.scaled))
head(sil_kmeans3[,1:3],4)
k3$cluster[1:4]
sil_kmeans.sum<-summary(sil_kmeans3)
sil_kmeans.sum$clus.avg.widths #Average silhouette width
sil_kmeans.sum$avg.width #Overall average silhouette
sil_kmeans.sum$clus.sizes #Size of each cluster
fviz_silhouette(sil_kmeans3,palette='jco',ggtheme=theme_classic()) 
fviz_nbclust(df.scaled,kmeans, method = "silhouette")+
  labs(title = "K-means")

#compare:K-medoids
sil_kmedoids <- silhouette(pam.res$cluster,dist(df.scaled))
summary(sil_kmedoids)
summary(sil_kmedoids)$clus.avg.widths 
summary(sil_kmedoids)$avg.width 
summary(sil_kmedoids)$clus.sizes 

fviz_nbclust(df.scaled, pam, method = "silhouette")+
  labs(title = "K-medoids with pam")
fviz_silhouette(sil_kmedoids,palette='jco',ggtheme=theme_classic()) 
#Both cluster1 and cluster2 have some negative signs


#3.Hierarchical clustering
## Find the best number of clusters using the elbow method
fviz_nbclust(df.scaled, FUNcluster = function(x, k) { list(cluster = cutree(hclust(dist(x)), k = k)) }, 
             method = "silhouette") +labs(title = "Hierarchical")
#fviz_nbclust(df.scaled, FUNcluster = hcut, method = "silhouette")+ labs(title = "Hierarchical")
#k_optimal=2

# Compute distances and hierarchical clustering
dd <- dist(df.scaled, method = "euclidean")
hc <- hclust(dd, method = "complete")
hc
hcut<-cutree(hc,k=2)
hcut
table(hcut) #cluster1:279,cluster2:161
rownames(df.scaled)[hcut == 1]

#We can visualise the object by using a dendrogram. 
#This will enable us to determine the point to cut the tree, 
#resulting in the number of clusters.
dendros <- as.dendrogram(hc)
plot(dendros, main = "Whole sale data - Complete linkage",
     ylab = "Height")
abline(h=0.5, lty = 2, col="red")
abline(h=1, lty = 2, col="blue")
Hs <- hc$height[(length(hc$height)-4):length(hc$height)]
abline(h=Hs, col=3, lty=2)

fviz_cluster(list(data=df.scaled, cluster=cutree(hc, 2)), ellipse.type = "norm")
fviz_dend(hc, k = 2, # Cut in two groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)

fviz_cluster(list(data=df.scaled, cluster=cutree(hc, 3)), ellipse.type = "norm")
fviz_dend(hc, k = 3, # Cut in two groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB",'red'),
          color_labels_by_k = TRUE, # color labels by groups
          ggtheme = theme_gray() # Change theme
)

hcut <- cutree(hc, k = 2)
table(hcut) #Check the number of observations in each of the two clusters
aggregate(d2, by=list(cluster=hcut), mean) 
#Can be used to calculate the mean of each variable by clusters using raw data

hc_silhouette_score <- mean(silhouette(hcut, dist(df.scaled))[, "sil_width"])
hc_silhouette_score #0.2223309

m <- c("average","single","complete")
names(m) <- c("average","single","complete")
# function to compute coefficient
ac <- function(x){
  agnes(df.scaled, method = x)$ac
}
map_dbl(m,ac) #complete: 0.9037892 

library(igraph)
fviz_dend(hc, k = 2, k_colors = "jco", type = "phylogenic", repel = TRUE)
fviz_dend(hc, k = 3, k_colors = "jco", type = "phylogenic", repel = TRUE)


#4.Hierarchical K-means Clustering
#First calculate the hierarchical clusters and cut the tree into k clusters, then calculate the centre (i.e. the mean) of each cluster, this is used as the initial centroid for k-means
res.hk2 <- hkmeans(df.scaled, k=2, hc.metric = "euclidean", hc.method ="complete")
res.hk2  #31.2 %
res.hk3 <- hkmeans(df.scaled, k=3, hc.metric = "euclidean", hc.method ="complete")
res.hk3 #41.7 %
res.hk4 <- hkmeans(df.scaled, k=4, hc.metric = "euclidean", hc.method ="complete")
res.hk4 #47.8 %

k2 #31.2 %
k3 #41.9 %
k4 #48.7%
#Basically the same as k-means, using hierarchical + k-means clustering
#without gaining any advantage



#5.Model-based hierarchical clustering: finding the maxbic
library(mclust)
mc2 <- Mclust(df.scaled, G = 2) # Model-based-clustering, G=2 indicates a request for 2 clusters
summary(mc2)
names(mc2)

mc3 <- Mclust(df.scaled, G = 3) # Model-based-clustering, G=3 indicates a request for 3 clusters
summary(mc3)

mc2$classification #Newly created clusters
plot(mc2, what = "classification")

?mclustModelNames

mc2_option_VVV <- Mclust(df.scaled, G = 2, modelNames = "VVV")
mc2_option_VVI <- Mclust(df.scaled, G = 2, modelNames = "VVI")
mc2_option_EVE <- Mclust(df.scaled, G = 2, modelNames = "EVE")
mc2_option_VEI <- Mclust(df.scaled, G = 2, modelNames = "VEI")

mc2_bic <- Mclust(df.scaled, G = 2)
mc2_bic$BIC #The first 3 model options based on the BIC standard
# VVE,2     VVV,2     EVE,2 
#-6359.651 -6407.469 -6428.712

mc3_bic <- Mclust(df.scaled, G = 3)
mc3_bic$BIC
#VVE,3     EVE,3     VVV,3 
#-6318.281 -6420.166 -6450.705 

mc4_bic <- Mclust(df.scaled, G = 4)
mc4_bic$BIC
#EVE,4     VVE,4     VEE,4 
#-6392.666 -6393.849 -6457.152 

G <- c(2,3,4,5,6)
modelNames <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", 
                'VEE','EVE','VVE','EVV',
                "EEV", "VEV", "VVV")

nnr <- length(G)*length(modelNames)
resu  <- array(data=NA, dim=c(nnr,3), dimnames=list(paste(1:nnr),c("G", "modelNames", "BIC")))
counter <- 1
for (i in G){
  for(j in modelNames){
    mc_option <- Mclust(df.scaled, G = i, modelNames = j)
    resu[counter, 1] <- as.numeric(i)
    resu[counter, 2] <- paste(j)
    resu[counter, 3] <- as.numeric(mc_option$BIC)
    if (counter < nrow(resu)) counter <- counter+1
  }
}
G <- as.numeric(resu[,1])
bic <- as.numeric(resu[,3])
model <- resu[,2]
dat <- data.frame(G, bic, model)

#Give the model combination with the maximum BIC and use the chosen combination to fit the final model
aa <- subset(dat, bic == max(bic))
aa
#G       bic model
#24 3 -6318.281   VVE
mcdf.scaled <- Mclust(df.scaled, G = 3, modelNames = "VVE") # Model-based-clustering
summary(mcdf.scaled )
#Clustering table:
#1   2   3 
#110 174 156 
plot(mcdf.scaled, what = "classification")
library(factoextra)
fviz_cluster(mcdf.scaled, data = df.scaled,
             palette = c("#2E9FDF", "#00AFBB","#E7B800"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)
mc_bic2 <- Mclust(df.scaled, G = 2)
mc_bic2$BIC
max(mc_bic2$BIC) #-6359.651

mc_bic3 <- Mclust(df.scaled, G = 3)
mc_bic3$BIC
max(mc_bic3$BIC) #-6318.281

mc_bic4 <- Mclust(df.scaled, G = 4)
mc_bic4$BIC
max(mc_bic4$BIC) #-6392.666

mc_bic5 <- Mclust(df.scaled, G = 5)
mc_bic5$BIC
max(mc_bic5$BIC) #-6404.392

mc_bic6 <- Mclust(df.scaled, G = 6)
mc_bic6$BIC
max(mc_bic6$BIC) #-6411.598

#In summary, the maximum bic value is VVE,3:-6318.281
#so we will use G=3 as the optimal number of clusters
#This is the same result we obtained in the for loop above


