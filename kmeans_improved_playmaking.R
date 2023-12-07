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

min_play <- minnesota %>%
  dplyr::select(HEIGHT,WEIGHT,AST,TOV,PF) %>%
  drop_na() %>%
  mutate(HEIGHT = (HEIGHT - mean(HEIGHT))/sd(HEIGHT),
         WEIGHT = (WEIGHT - mean(WEIGHT))/sd(WEIGHT),
         AST = (AST - mean(AST))/sd(AST),
         TOV = (TOV - mean(TOV))/sd(TOV),
         PF = (PF - mean(PF))/sd(PF))


######## GATHERING STATS AT DIFFERENT NUMBER OF CLUSTERS ######## 

set.seed(627) # set seed to ensure reproduce ability b/c k-means relies on random states for initialization 

MAX_K <- 20 # max number of clusters
wss.p <- c() # vector to hold wss.p of each model
ratio_play <- c() # vector to hold BSS/TSS ratio_play of each model

# k means for 1-20 clusters
for (k in 1:MAX_K) {
  algo_k_p <- kmeans(min_play, centers=k, nstart=100, iter.max=20) # k-means algorithm
  wss.p <- c(wss.p, algo_k_p$tot.withinss) # get wss.p
  ratio_play <- c(ratio_play, algo_k_p$betweenss/algo_k_p$totss) # get ratio_play
} 



######## VISUALIZING wss.p ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 


# datasets for graphs

wss.p.k <- data.frame(k = 1:MAX_K, wss.p = wss.p)

wss.p.k.diff1 <- data.frame(k = 1:MAX_K, wss.p_difference = wss.p-lead(wss.p)) %>%
  dplyr::filter(k<MAX_K-1)

wss.p.k.diff2 <- data.frame(k = 1:MAX_K, wss.p_difference = wss.p-2*lead(wss.p)+lead(wss.p, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# color blind palette for graphs
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# WSS Plot (No Rolling Distance)
ggplot(data = wss.p.k, aes(x=k, y=wss.p)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + # set color of point and lines
  labs(x = "K", y = "WSS", title = "WSS Across K-Clusters") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # playine x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  scale_colour_manual(values=cbPalette)# manually alter theme

# 1-Unit Rolling Distance WSS Graph
ggplot(data = wss.p.k.diff1, aes(x=k, y=wss.p_difference)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 6, y = wss.p_difference[6]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "One-Unit Rolling Distance of WSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance WSS Graph
wss_play_2 <- ggplot(data = wss.p.k.diff2, aes(x=k, y=wss.p_difference)) + 
  geom_point(color=cbPalette[3]) + 
  geom_line(color=cbPalette[1]) + 
  geom_point(aes(x = 6, y = wss.p_difference[6]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "WSS", title = "Two-Unit Rolling Distance of WSS Across K-Playmaking Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())



######## VISUALIZING BSS/TSS ACROSS DIFFERENT NUMBER OF CLUSTERS ######## 

# Creating datasets to graph with 

ratio_play.k <- data.frame(k = 1:MAX_K, RATIO = ratio_play)

ratio_play.k.diff1 <- data.frame(k = 1:MAX_K, RATIO = ratio_play-lead(ratio_play)) %>%
  dplyr::filter(k<MAX_K-1)

ratio_play.k.diff2 <- data.frame(k = 1:MAX_K, RATIO = ratio_play-2*lead(ratio_play)+lead(ratio_play, 2)) %>%
  dplyr::filter(k<MAX_K-1)

# BSS/TSS Graph
ggplot(data = ratio_play.k, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 6, y = RATIO[6]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 1-Unit Rolling Distance BSS/TSS Graph
ggplot(data = ratio_play.k.diff1, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 6, y = RATIO[6]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "One-Unit Rolling Distance of BSS/TSS Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# 2-Unit Rolling Distance BSS/TSS Graph
bss_play_2 <- ggplot(data = ratio_play.k.diff2, aes(x=k, y=RATIO)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 6, y = RATIO[6]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "BSS/TSS", title = "Two-Unit Rolling Distance of BSS/TSS Across K-Playmaking Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

ggarrange(wss_play_2, bss_play_2)
######## CLUSTERING DATA WITH K=6 CLUSTERS ######## 
cluster_names <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                   'Cluster 4', 'Cluster 5', 'Cluster 6',
                   'Cluster 7', 'Cluster 8', 'Cluster 9',
                   'Cluster 10', 'Cluster 11', 'Cluster 12')


# Run K-Means On K=6 Clusters 
set.seed(627)
K <- 6

k.m.play <- kmeans(min_play, centers=K, nstart=100, iter.max=20)

######## VISUALIZING K=6 CLUSTER CENTROIDS ######## 

# Find scaled cluster centroids
km_centers_play <- as.data.frame(k.m.play$centers) # SCALED cluster centers/means

### Organize Cluster Centroids for Graphing ###

# name clusters before pivoting
km_centers_play$Cluster <- cluster_names[1:6]

# massage data
km_centers_play <- km_centers_play %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers_play$feature <- factor(km_centers_play$feature, levels=c("HEIGHT","WEIGHT","AST","TOV", "PF")) 

# reset the order of clusters for plotting (cluster 10 would playault to come after cluster 1 and before cluster 2)
km_centers_play$Cluster <- factor(km_centers_play$Cluster, levels=cluster_names[1:6])

# Point Graphs for feature means across K = 7 Clusters
km_centers_play %>% 
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
ggplot(km_centers_play, aes(x=feature, y=z_val, color=Cluster, group = Cluster)) + 
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
pca_play <- prcomp(min_play, scale = FALSE) # perform Principle Component Analysis 
pca_play_summary <- summary(pca_play) # summary of PCA model

# find the importance scores for PCA summary 
importance_play <- data.frame(imp = pca_play_summary$importance[2,], n = 1:length(pca_play_summary$importance[2,]))

# plot % of variance between players explained by each subsequent PC 
ggplot(data = importance_play, aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

# Get data for plotting clusters across PC's
pc2_play <- as.data.frame(pca_play$x[,1:2]) # extract first two PCs
pc2_play$Cluster <- as.factor(k.m.play$cluster) # add player clusters 
cluster1_play_var <- round(pca_play_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_play_var <- round(pca_play_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

find_hull <- function(pc2_play) pc2_play[chull(pc2_play$PC2, pc2_play$PC1), ]
hulls <- ddply(pc2_play, "Cluster", find_hull)

# Graph of Clusters across first two PC's
pc2_play %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster, fill = Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + 
  scale_shape_manual(values=seq(0,15)) + 
  geom_mark_ellipse(expand = 0, alpha = .05) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_play_var, '% of Variance)'), # playine cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_play_var, '% of Variance)'), # playine cluster 2 % of variance
       title = 'K-Means Cluster Differences Across First Two Principle Components') +
  ggpubr::stat_mean(aes(color = Cluster), size = 4, shape = 16)


######## CLUSTER ANALYSIS ######## 


minnesota_clusters_play <- cbind(minnesota, playmaking_cluster = as.factor(k.m.play$cluster))

playmaking_cluster_raw_centers <- minnesota_clusters_play %>%
  group_by(playmaking_cluster) %>%
  dplyr::summarise(HEIGHT = mean(HEIGHT),
                   WEIGHT = mean(WEIGHT),
                   AST = mean(AST),
                   TOV = mean(TOV),
                   PF = mean(PF))

height_plot <- ggplot(playmaking_cluster_raw_centers, aes(x=playmaking_cluster, y=HEIGHT, color=playmaking_cluster, group = playmaking_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Height (in) Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())


weight_plot <- ggplot(playmaking_cluster_raw_centers, aes(x=playmaking_cluster, y=WEIGHT, color=playmaking_cluster, group = playmaking_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Weight Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

assists_plot <- ggplot(playmaking_cluster_raw_centers, aes(x=playmaking_cluster, y=AST, color=playmaking_cluster, group = playmaking_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Assists Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

turnover_plot <- ggplot(playmaking_cluster_raw_centers, aes(x=playmaking_cluster, y=TOV, color=playmaking_cluster, group = playmaking_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Turnover Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())

fouls_plot <- ggplot(playmaking_cluster_raw_centers, aes(x=playmaking_cluster, y=PF, color=playmaking_cluster, group = playmaking_cluster)) + 
  geom_point(alpha = .5, size = 3) + # plot points
  labs(x = "Cluster", y = "Cluster Center", 
       title = "Personal Fouls Across Clusters") +
  theme_minimal() + 
  theme(strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())


ggarrange(height_plot, weight_plot, assists_plot, turnover_plot,fouls_plot)

# Cluster 1: short, very low weight, very high assists, very high turnover, avg pf
# Cluster 2: very short, very low weight, below avg assists, low turnover, low pf
# Cluster 3: avg height, avg weight, very low assists, very low turnover, very low pf
# Cluster 4: very tall, weights a lot, avg assists, very high turnover, very high pf
# Cluster 5: above avg height, very high weight, low assists, low turnover, below avg pf
# Cluster 6: avg height, below avg weight, low assists, avg turnover, avg pf


min_playmaking_clusters <- minnesota
min_playmaking_clusters$playmaking_cluster <- as.factor(k.m.play$cluster)
min_playmaking_clusters$EFF <- (min_playmaking_clusters$PTS + 
                                  min_playmaking_clusters$REB + 
                                  min_playmaking_clusters$AST +
                                  min_playmaking_clusters$STL + 
                                  min_playmaking_clusters$BLK - 
                                  (min_playmaking_clusters$FGA - min_playmaking_clusters$FG_PCT) -
                                  (min_playmaking_clusters$FTA - min_playmaking_clusters$FT_PCT) - 
                                  min_playmaking_clusters$TOV) / min_playmaking_clusters$GP
min_playmaking_clusters %>%
  group_by(playmaking_cluster) %>%
  dplyr::summarise(size = n(),
                   USAGE = mean(USG_PCT),
                   POSSESSIONS = mean(POSS),
                   EFF = mean(EFF),
                   HEIGHT = mean(HEIGHT),
                   WEIGHT = mean(WEIGHT),
                   AST = mean(AST),
                   TOV = mean(TOV),
                   PF = mean(PF)) %>%
  view()



