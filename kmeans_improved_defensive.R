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
library(plyr)
library(ggpubr)

######## GETTING DATA ######## 
minnesota <- read_csv('minnesota_stats_newvar.csv')
minnesota <- minnesota[,c(2:ncol(minnesota))] 

######## NORMALIZING ######## 
min_defensive <- minnesota %>%
  dplyr::select(CONTESTED_SHOTS,REB,STL,BLK) %>%
  drop_na() %>%
  mutate(CONTESTED_SHOTS = (CONTESTED_SHOTS - mean(CONTESTED_SHOTS))/sd(CONTESTED_SHOTS),
         REB = (REB - mean(REB))/sd(REB),
         STL = (STL - mean(STL))/sd(STL),
         BLK = (BLK - mean(BLK))/sd(BLK))



######## GATHERING STATS AT DIFFERENT NUMBER OF CLUSTERS ######## 

set.seed(627) # set seed to ensure reproduce ability b/c k-means relies on random states for initialization 

MAX_K <- 20 # max number of clusters
wss.d <- c() # vector to hold wss.d of each model
ratio_def <- c() # vector to hold BSS/TSS ratio_def of each model

# k means for 1-20 clusters
for (k in 1:MAX_K) {
  algo_k_d <- kmeans(min_defensive, centers=k, nstart=100, iter.max=20) # k-means algorithm
  wss.d <- c(wss.d, algo_k_d$tot.withinss) # get wss.d
  ratio_def <- c(ratio_def, algo_k_d$betweenss/algo_k_d$totss) # get ratio_def
} 



######## VISUALIZING WSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 


# datasets for graphs

wss.d.k <- data.frame(k = 1:MAX_K, wss.d = wss.d)

wss.d.k.diff1 <- data.frame(k = 1:MAX_K, wss.d_difference = wss.d-lead(wss.d)) %>%
  dplyr::filter(k<MAX_K-1)

wss.d.k.diff2 <- data.frame(k = 1:MAX_K, wss.d_difference = wss.d-2*lead(wss.d)+lead(wss.d, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# color blind palette for graphs
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# WSS Plot (No Rolling Distance)
ggplot(data = wss.d.k, aes(x=k, y=wss.d)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + # set color of point and lines
  labs(x = "K", y = "WSS", title = "WSS Across K-Clusters") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  scale_colour_manual(values=cbPalette)# manually alter theme

# 1-Unit Rolling Distance WSS Graph
ggplot(data = wss.d.k.diff1, aes(x=k, y=wss.d_difference)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 7, y = wss.d_difference[7]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "One-Unit Rolling Distance of WSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance WSS Graph
wss_def_2 <- ggplot(data = wss.d.k.diff2, aes(x=k, y=wss.d_difference)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + 
  geom_point(aes(x = 5, y = wss.d_difference[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "Two-Unit Rolling Distance of WSS Across K-Defensive Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())



######## VISUALIZING BSS/TSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 

# Creating datasets to graph with 

ratio_def.k <- data.frame(k = 1:MAX_K, RATIO = ratio_def)

ratio_def.k.diff1 <- data.frame(k = 1:MAX_K, RATIO = ratio_def-lead(ratio_def)) %>%
  dplyr::filter(k<MAX_K-1)

ratio_def.k.diff2 <- data.frame(k = 1:MAX_K, RATIO = ratio_def-2*lead(ratio_def)+lead(ratio_def, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# BSS/TSS Graph
ggplot(data = ratio_def.k, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 1-Unit Rolling Distance BSS/TSS Graph
ggplot(data = ratio_def.k.diff1, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "One-Unit Rolling Distance of BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance BSS/TSS Graph
bss_def_2 <- ggplot(data = ratio_def.k.diff2, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "Two-Unit Rolling Distance of BSS/TSS Across K-Defensive Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

ggarrange(wss_def_2, bss_def_2)
######## CLUSTERING DATA WITH K=7 CLUSTERS ######## 
cluster_names <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                  'Cluster 4', 'Cluster 5', 'Cluster 6',
                  'Cluster 7', 'Cluster 8', 'Cluster 9',
                  'Cluster 10', 'Cluster 11', 'Cluster 12')


# Run K-Means On K=7 Clusters 
set.seed(627)
K <- 5

k.m.def <- kmeans(min_defensive, centers=K, nstart=100, iter.max=20)

######## VISUALIZING K=7 CLUSTER CENTROIDS ######## 

# Find scaled cluster centroids
km_centers_def <- as.data.frame(k.m.def$centers) # SCALED cluster centers/means

### Organize Cluster Centroids for Graphing ###

# name clusters before pivoting
km_centers_def$Cluster <- cluster_names[1:5]

# massage data
km_centers_def <- km_centers_def %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers_def$feature <- factor(km_centers_def$feature, levels=c("CONTESTED_SHOTS","REB","STL","BLK")) 

# reset the order of clusters for plotting (cluster 10 would default to come after cluster 1 and before cluster 2)
km_centers_def$Cluster <- factor(km_centers_def$Cluster, levels=cluster_names[1:5])

# Point Graphs for feature means across K = 7 Clusters
km_centers_def %>% 
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
ggplot(km_centers_def, aes(x=feature, y=z_val, color=Cluster, group = Cluster)) + 
  geom_point(alpha = .5) + # plot points
  geom_line(aes(y = z_val, color = Cluster)) + # plot lines 
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Across Features") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())



######## USING PCA TO VISUALIZE K=7 CLUSTERS ######## 

# run PCA on data 
pca_def <- prcomp(min_defensive, scale = FALSE) # perform Principle Component Analysis 
pca_def_summary <- summary(pca_def) # summary of PCA model

# find the importance scores for PCA summary 
importance_def <- data.frame(imp = pca_def_summary$importance[2,], n = 1:length(pca_def_summary$importance[2,]))

# plot % of variance between players explained by each subsequent PC 
ggplot(data = importance_def, aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Get data for plotting clusters across PC's
pc2_def <- as.data.frame(pca_def$x[,1:2]) # extract first two PCs
pc2_def$Cluster <- as.factor(k.m.def$cluster) # add player clusters 
cluster1_def_var <- round(pca_def_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_def_var <- round(pca_def_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

find_hull <- function(pc2_def) pc2_def[chull(pc2_def$PC2, pc2_def$PC1), ]
hulls <- ddply(pc2_def, "Cluster", find_hull)

# Graph of Clusters across first two PC's
pc2_def %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster, fill = Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + 
  scale_shape_manual(values=seq(0,15)) + 
  geom_mark_ellipse(expand = 0, alpha = .05) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_def_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_def_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'K-Means Cluster Differences Across First Two Principle Components') +
  ggpubr::stat_mean(aes(color = Cluster), size = 4, shape = 16)



######## CLUSTER ANALYSIS ######## 

minnesota_clusters_def <- cbind(minnesota, defensive_cluster = as.factor(k.m.def$cluster))


defensive_cluster_raw_centers <- minnesota_clusters_def %>%
  group_by(defensive_cluster) %>%
  dplyr::summarise(CONTESTED_SHOTS = mean(CONTESTED_SHOTS),
            REB = mean(REB),
            STL = mean(STL),
            BLK = mean(BLK))

shots_plot <- ggplot(defensive_cluster_raw_centers, aes(x=defensive_cluster, y=CONTESTED_SHOTS, color=defensive_cluster, group = defensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Contested Shots Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

reb_plot <- ggplot(defensive_cluster_raw_centers, aes(x=defensive_cluster, y=REB, color=defensive_cluster, group = defensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Rebounds Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

steal_plot <- ggplot(defensive_cluster_raw_centers, aes(x=defensive_cluster, y=STL, color=defensive_cluster, group = defensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Steals Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

block_plot <- ggplot(defensive_cluster_raw_centers, aes(x=defensive_cluster, y=BLK, color=defensive_cluster, group = defensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Blocks Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

ggarrange(shots_plot, reb_plot, steal_plot, block_plot)



# Cluster 1: low shots, low rebounds, avg steals, below avg blocks
# Cluster 2: below avg shots, below avg rebounds, very high steals, below avg blocks
# Cluster 3: high shots, avg rebounds, high steals, high blocks
# Cluster 4: avg shots, very high rebounds, avg steals, very high blocks
# Cluster 5: below avg shots, below avg rebounds, avg steals, avg blocks
# Cluster 6: low shots, low rebounds, very low steals, low blocks
# Cluster 7: very high shots, very low rebounds, low steals, very low (almost 0) blocks

min_defensive_clusters <- minnesota
min_defensive_clusters$defensive_cluster <- as.factor(k.m.def$cluster)

min_defensive_clusters %>%
  group_by(defensive_cluster) %>%
  dplyr::summarise(size = n(),
                   DEF_RATING = mean(DEF_RATING),
                   DEFENSIVE_WINSHARE = mean(DEF_WS),
                   CONTESTED_SHOTS = mean(CONTESTED_SHOTS),
                   REB = mean(REB),
                   STL = mean(STL),
                   BLK = mean(BLK)) %>%
  view()



  
  


