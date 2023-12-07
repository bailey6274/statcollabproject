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

######## GETTING DATA ######## 
minnesota <- read_csv('minnesota_stats_newvar.csv')
minnesota <- minnesota[,c(2:ncol(minnesota))] 

######## NORMALIZING ######## 

min_offensive <- minnesota %>%
  dplyr::select(SHOT_DISTANCE,FGM,FG3M,FTM,PTS,FGA,FG3A,FTA) %>%
  drop_na() %>%
  mutate(SHOT_DISTANCE = (SHOT_DISTANCE - mean(SHOT_DISTANCE))/sd(SHOT_DISTANCE),
         FGM = (FGM - mean(FGM))/sd(FGM),
         FG3M = (FG3M - mean(FG3M))/sd(FG3M),
         FTM = (FTM - mean(FTM))/sd(FTM),
         PTS = (PTS - mean(PTS))/sd(PTS),
         FGA = (FGA - mean(FGA))/sd(FGA),
         FG3A = (FG3A - mean(FG3A))/sd(FG3A),
         FTA = (FTA - mean(FTA))/sd(FTA))

######## GATHERING STATS AT DIFFERENT NUMBER OF CLUSTERS ######## 

set.seed(627) # set seed to ensure reproduce ability b/c k-means relies on random states for initialization 

MAX_K <- 20 # max number of clusters
wss.o <- c() # vector to hold wss.o of each model
ratio_off <- c() # vector to hold BSS/TSS ratio_off of each model

# k means for 1-20 clusters
for (k in 1:MAX_K) {
  algo_k_off <- kmeans(min_offensive, centers=k, nstart=100, iter.max=20) # k-means algorithm
  wss.o <- c(wss.o, algo_k_off$tot.withinss) # get wss.o
  ratio_off <- c(ratio_off, algo_k_off$betweenss/algo_k_off$totss) # get ratio_off
} 



######## VISUALIZING WSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 


# datasets for graphs

wss.o.k <- data.frame(k = 1:MAX_K, wss.o = wss.o)

wss.o.k.diff1 <- data.frame(k = 1:MAX_K, wss.o_difference = wss.o-lead(wss.o)) %>%
  dplyr::filter(k<MAX_K-1)

wss.o.k.diff2 <- data.frame(k = 1:MAX_K, wss.o_difference = wss.o-2*lead(wss.o)+lead(wss.o, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# color blind palette for graphs
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# WSS Plot (No Rolling Distance)
ggplot(data = wss.o.k, aes(x=k, y=wss.o)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + # set color of point and lines
  labs(x = "K", y = "WSS", title = "WSS Across K-Clusters") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # playine x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  scale_colour_manual(values=cbPalette)# manually alter theme

# 1-Unit Rolling Distance WSS Graph
ggplot(data = wss.o.k.diff1, aes(x=k, y=wss.o_difference)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = wss.o_difference[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "One-Unit Rolling Distance of WSS Across K-Offensive Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance WSS Graph
wss_off_2 <- ggplot(data = wss.o.k.diff2, aes(x=k, y=wss.o_difference)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + 
  geom_point(aes(x = 5, y = wss.o_difference[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "Two-Unit Rolling Distance of WSS Across K-Offensive Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())



######## VISUALIZING BSS/TSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 

# Creating datasets to graph with 

ratio_off.k <- data.frame(k = 1:MAX_K, RATIO = ratio_off)

ratio_off.k.diff1 <- data.frame(k = 1:MAX_K, RATIO = ratio_off-lead(ratio_off)) %>%
  dplyr::filter(k<MAX_K-1)

ratio_off.k.diff2 <- data.frame(k = 1:MAX_K, RATIO = ratio_off-2*lead(ratio_off)+lead(ratio_off, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# BSS/TSS Graph
ggplot(data = ratio_off.k, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 1-Unit Rolling Distance BSS/TSS Graph
ggplot(data = ratio_off.k.diff1, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "One-Unit Rolling Distance of BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance BSS/TSS Graph
bss_off_2 <- ggplot(data = ratio_off.k.diff2, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 5, y = RATIO[5]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "Two-Unit Rolling Distance of BSS/TSS Across K-Offensive Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

ggarrange(wss_off_2, bss_off_2)

######## CLUSTERING DATA WITH K=5 CLUSTERS ######## 
cluster_names <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                   'Cluster 4', 'Cluster 5', 'Cluster 6',
                   'Cluster 7', 'Cluster 8', 'Cluster 9',
                   'Cluster 10', 'Cluster 11', 'Cluster 12')


# Run K-Means On K=7 Clusters 
set.seed(627)
K <- 5

k.m.off <- kmeans(min_offensive, centers=K, nstart=100, iter.max=20)

######## VISUALIZING K=5 CLUSTER CENTROIDS ######## 

# Find scaled cluster centroids
km_centers_off <- as.data.frame(k.m.off$centers) # SCALED cluster centers/means

### Organize Cluster Centroids for Graphing ###

# name clusters before pivoting
km_centers_off$Cluster <- cluster_names[1:5]

# massage data
km_centers_off <- km_centers_off %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers_off$feature <- factor(km_centers_off$feature, levels=c("SHOT_DISTANCE","FGM","FG3M","FTM","PTS","FGA","FG3A","FTA")) 

# reset the order of clusters for plotting (cluster 10 would playault to come after cluster 1 and before cluster 2)
km_centers_off$Cluster <- factor(km_centers_off$Cluster, levels=cluster_names[1:5])

# Point Graphs for feature means across K = 6 Clusters
km_centers_off %>% 
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
ggplot(km_centers_off, aes(x=feature, y=z_val, color=Cluster, group = Cluster)) + 
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
pca_off <- prcomp(min_offensive, scale = FALSE) # perform Principle Component Analysis 
pca_off_summary <- summary(pca_off) # summary of PCA model

# find the importance scores for PCA summary 
importance_off <- data.frame(imp = pca_off_summary$importance[2,], n = 1:length(pca_off_summary$importance[2,]))

# plot % of variance between players explained by each subsequent PC 
ggplot(data = importance_off, aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Get data for plotting clusters across PC's
pc2_off <- as.data.frame(pca_off$x[,1:2]) # extract first two PCs
pc2_off$Cluster <- as.factor(k.m.off$cluster) # add player clusters 
cluster1_off_var <- round(pca_off_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_off_var <- round(pca_off_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

find_hull <- function(pc2_off) pc2_off[chull(pc2_off$PC2, pc2_off$PC1), ]
hulls <- ddply(pc2_off, "Cluster", find_hull)

# Graph of Clusters across first two PC's
pc2_off %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster, fill = Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + 
  scale_shape_manual(values=seq(0,15)) + 
  geom_mark_ellipse(expand = 0, alpha = .05) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_off_var, '% of Variance)'), # playine cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_off_var, '% of Variance)'), # playine cluster 2 % of variance
       title = 'K-Means Cluster Differences Across First Two Principle Components') +
  ggpubr::stat_mean(aes(color = Cluster), size = 4, shape = 16)


######## CLUSTER ANALYSIS ######## 

minnesota_clusters_offensive <- cbind(minnesota, offensive_cluster = as.factor(k.m.off$cluster))

offensive_cluster_raw_centers <- minnesota_clusters_offensive %>%
  group_by(offensive_cluster) %>%
  dplyr::summarise(SHOT_DISTANCE = mean(SHOT_DISTANCE),
                   FGM = mean(FGM),
                   FG3M = mean(FG3M),
                   FTM = mean(FTM),
                   PTS = mean(PTS),
                   FGA = mean(FGA), 
                   FG3A = mean(FG3A),
                   FTA = mean(FGA))

shotdist_plot <- ggplot(offensive_cluster_raw_centers, aes(x=offensive_cluster, y=SHOT_DISTANCE, color=offensive_cluster, group = offensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Shot Distance Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

fieldgoal_plot <- ggplot(offensive_cluster_raw_centers, aes(x=offensive_cluster, y=FGM, color=offensive_cluster, group = offensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Field Goal Percentage Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

fieldgoal_3pt_plot <- ggplot(offensive_cluster_raw_centers, aes(x=offensive_cluster, y=FG3M, color=offensive_cluster, group = offensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Three-Point Feild Goal Percentage Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

freethrow_plot <- ggplot(offensive_cluster_raw_centers, aes(x=offensive_cluster, y=FTM, color=offensive_cluster, group = offensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Free Throw Percentage Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

points_plot <- ggplot(offensive_cluster_raw_centers, aes(x=offensive_cluster, y=PTS, color=offensive_cluster, group = offensive_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Points Scored Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())


ggarrange(shotdist_plot, fieldgoal_plot, fieldgoal_3pt_plot, freethrow_plot, points_plot)

# Cluster 1: avg shot dist, above avg fg, above avg fg3, above avg ft, avg pts
# Cluster 2: above avg shot dist, avg fg, above avg fg3, above avg ft, avg pts
# Cluster 3: high shot dist, very high fg, very high fg3, very low ft, below avg pts
# Cluster 4: very high shot dist, very low fg, low fg3, low ft, below avg pts
# Cluster 5: above avg shot dist, above avg fg, above avg fg3, high ft, very high pts
# Cluster 6: below avg shot dist, above avg fg, below avg fg3, avg ft, avg pts

min_offensive_clusters <- minnesota
min_offensive_clusters$offensive_cluster <- as.factor(k.m.off$cluster)

min_offensive_clusters %>%
  group_by(offensive_cluster) %>%
  dplyr::summarise(size = n(),
                   OFFENSIVE_RATING = mean(OFF_RATING),
                   PLUS_MINUS = mean(PLUS_MINUS),
                   PTS= mean(PTS),
                   SHOT_DISTANCE = mean(SHOT_DISTANCE),
                   FGM = mean(FGM),
                   FG3M = mean(FG3M),
                   FTM = mean(FTM),
                   FGA = mean(FGA), 
                   FG3A = mean(FG3A),
                   FTA = mean(FGA),
                   GAMES_PLAYED = mean(GP)) %>%
  view()

min_offensive_clusters %>%
  group_by(PLAYER_NAME, offensive_cluster) %>%
  dplyr::summarise(size = n(),
                   OFFENSIVE_RATING = mean(OFF_RATING),
                   PLUS_MINUS = mean(PLUS_MINUS)) %>%
  view()

min_offensive_clusters %>% filter(offensive_cluster ==1) %>% view()
