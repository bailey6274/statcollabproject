library(tidyverse)
library(mosaic)
library(knitr)
library(ggplot2)
library(arm)
library(broom)
library(boot)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(psych)
library(pROC)
library(klaR)
library(factoextra)
library(plyr)
library(ggforce)

######## GETTING DATA ######## 
source("create_dataset.R")

######## NORMALIZING ######## 

min <- minnesota %>%
  dplyr::select(HEIGHT,WEIGHT,FG_PCT,FG3_PCT,FT_PCT,REB,AST,TOV,PTS,STL,AGE,BLK) %>%
  drop_na() %>%
  mutate(HEIGHT = (HEIGHT - mean(HEIGHT))/sd(HEIGHT),
         WEIGHT = (WEIGHT - mean(WEIGHT))/sd(WEIGHT),
         FG_PCT = (FG_PCT - mean(FG_PCT))/sd(FG_PCT),
         FG3_PCT = (FG3_PCT - mean(FG3_PCT))/sd(FG3_PCT),
         FT_PCT = (FT_PCT - mean(FT_PCT))/sd(FT_PCT),
         REB = (REB - mean(REB))/sd(REB),
         AST = (AST - mean(AST))/sd(AST),
         TOV = (TOV - mean(TOV))/sd(TOV),
         PTS = (PTS - mean(PTS))/sd(PTS),
         STL = (STL - mean(STL))/sd(STL),
         AGE = (AGE - mean(AGE))/sd(AGE),
         BLK = (BLK - mean(BLK))/sd(BLK))

######## GATHERING STATS AT DIFFERENT NUMBER OF CLUSTERS ######## 

set.seed(2002) # set seed to ensure reproduce ability b/c k-means relies on random states for initialization 

MAX_K <- 20 # max number of clusters
wss <- c() # vector to hold WSS of each model
ratio <- c() # vector to hold BSS/TSS ratio of each model

# k means for 1-20 clusters
for (k in 1:MAX_K) {
  algo_k <- kmeans(min, centers=k, nstart=100, iter.max=20) # k-means algorithm
  wss <- c(wss, algo_k$tot.withinss) # get WSS
  ratio <- c(ratio, algo_k$betweenss/algo_k$totss) # get ratio
} 


######## VISUALIZING WSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 


# datasets for graphs

wss.k <- data.frame(k = 1:MAX_K, WSS = wss)

wss.k.diff1 <- data.frame(k = 1:MAX_K, WSS_difference = wss-lead(wss)) %>%
  dplyr::filter(k<MAX_K-1)

wss.k.diff2 <- data.frame(k = 1:MAX_K, WSS_difference = wss-2*lead(wss)+lead(wss, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# color blind palette for graphs
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# WSS Plot (No Rolling Distance)
ggplot(data = wss.k, aes(x=k, y=WSS)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + # set color of point and lines
  labs(x = "K", y = "WSS", title = "WSS Across K-Clusters") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  scale_colour_manual(values=cbPalette)# manually alter theme

# 1-Unit Rolling Distance WSS Graph
ggplot(data = wss.k.diff1, aes(x=k, y=WSS_difference)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 7, y = WSS_difference[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "One-Unit Rolling Distance of WSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance WSS Graph
ggplot(data = wss.k.diff2, aes(x=k, y=WSS_difference)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + 
  geom_point(aes(x = 7, y = WSS_difference[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "Two-Unit Rolling Distance of WSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())


######## VISUALIZING BSS/TSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 

# Creating datasets to graph with 

ratio.k <- data.frame(k = 1:MAX_K, RATIO = ratio)

ratio.k.diff1 <- data.frame(k = 1:MAX_K, RATIO = ratio-lead(ratio)) %>%
  dplyr::filter(k<MAX_K-1)

ratio.k.diff2 <- data.frame(k = 1:MAX_K, RATIO = ratio-2*lead(ratio)+lead(ratio, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# BSS/TSS Graph
ggplot(data = ratio.k, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 7, y = RATIO[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 1-Unit Rolling Distance BSS/TSS Graph
ggplot(data = ratio.k.diff1, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 7, y = RATIO[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "One-Unit Rolling Distance of BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance BSS/TSS Graph
ggplot(data = ratio.k.diff2, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 7, y = RATIO[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "Two-Unit Rolling Distance of BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())


######## CLUSTERING DATA WITH K=7 CLUSTERS ######## 

# Run K-Means On K=7 Clusters 
set.seed(2002)
K <- 6

k.m <- kmeans(min, centers=K, nstart=100, iter.max=20)

######## VISUALIZING K=7 CLUSTER CENTROIDS ######## 

# Find scaled cluster centroids
km_centers <- as.data.frame(k.m$centers) # SCALED cluster centers/means

### Organize Cluster Centroids for Graphing ###

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6') 

# massage data
km_centers <- km_centers %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers$feature <- factor(km_centers$feature, levels=c("HEIGHT","WEIGHT","FG_PCT","FG3_PCT","FT_PCT","REB","AST","TOV","PTS","STL","AGE","BLK")) 

# reset the order of clusters for plotting (cluster 10 would default to come after cluster 1 and before cluster 2)
km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6'))

# Point Graphs for feature means across K = 7 Clusters
km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight::gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_minimal() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

# Line Graph for cluster trends across features 
ggplot(km_centers, aes(x=feature, y=z_val, color=Cluster, group = Cluster)) + 
  geom_point(alpha = .5) + # plot points
  geom_line(aes(y = z_val, color = Cluster)) + # plot lines 
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Across Features") +
  theme_minimal() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())





######## USING PCA TO VISUALIZE K=7 CLUSTERS ######## 

# run PCA on data 
pca <- prcomp(min, scale = FALSE) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

# find the importance scores for PCA summary 
importance <- data.frame(imp = pca_summary$importance[2,], n = 1:length(pca_summary$importance[2,]))

# plot % of variance between players explained by each subsequent PC 
ggplot(data = importance, aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Get data for plotting clusters across PC's
pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(k.m$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

find_hull <- function(pc2) pc2[chull(pc2$PC2, pc2$PC1), ]
hulls <- ddply(pc2, "Cluster", find_hull)

# Graph of Clusters across first two PC's
pc2 %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster, fill = Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + 
  scale_shape_manual(values=seq(0,15)) + 
  geom_mark_ellipse(expand = 0, alpha = .05) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'K-Means Cluster Differences Across First Two Principle Components') +
  ggpubr::stat_mean(aes(color = Cluster), size = 4, shape = 16)







