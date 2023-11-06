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
library(gghighlight)




source('create_dataset.R')

colnames(minnesota)
minnesota_stats %>%
  left_join(minnesota_roster, by = c("PLAYER_ID" = "PLAYER_ID")) %>%
  dplyr::select(HEIGHT, WEIGHT, FG_PCT) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

ggplot(minnesota, aes(x = HEIGHT, y = FG_PCT, color = POSITION)) + 
  geom_point()


minnesota_stats %>%
  left_join(minnesota_roster, by = c("PLAYER_ID" = "PLAYER_ID")) %>%
  filter(POSITION == "G") %>%
  group_by(PLAYER_ID) %>%
  summarize(avg_points = mean(PTS),
            avg_rebounds = mean(REB),
            avg_assis = mean(AST),
            avg_fg_pct = mean(FG_PCT),
            avg_fg_3_pct = mean(FG3_PCT),
            
            ) %>%
  view()





kmeans(minnesota[,c("HEIGHT", "FG_PCT")], centers = 3, nstart = 20)

kmodes(minnesota[,c("HEIGHT", "POSITION", "FG_PCT")], 3, iter.max = 100)


minnesota %>%
  dplyr::select(PLAYER_ID, SEASON_ID, PLAYER_AGE, PLAYER, POSITION, HEIGHT, WEIGHT, EXP, SCHOOL,
                GP, GS, MIN, FGM, FGA, FG_PCT, FG3M, FG3A, FG3_PCT, 
                FTM, FTA, FT_PCT, OREB, DREB, REB, AST, STL, BLK, TOV, PF, PTS)


min_scale <- minnesota %>%
  dplyr::select(HEIGHT,WEIGHT,FG_PCT,FG3_PCT,FT_PCT,REB,AST,TOV,PTS,STL,AGE,BLK) %>%
  drop_na() %>%
  scale()



set.seed(2002) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(min_scale, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 



tibble(k = 1:MAX_K, SSE = sse) %>%
  ggplot(aes(x=k, y=SSE)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE", title = "Where does this level off?") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # manually alter theme

tibble(k = 1:MAX_K, SSE_difference = sse-lead(sse)) %>%
  dplyr::filter(k<MAX_K) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE Rolling Difference", title = "A Clearer Picture") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal()  + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # manually alter theme


tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())


K <- 8

kmeans8 <- kmeans(min_scale, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans8$centers) # SCALED cluster centers/means

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6',
                        'Cluster 7', 'Cluster 8') 
# massage data
km_centers <- km_centers %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers$feature <- factor(km_centers$feature, levels=c("HEIGHT","WEIGHT","FG_PCT","FG3_PCT","FT_PCT","REB","AST","TOV","PTS","STL","AGE","BLK")) 

# reset the order of clusters for plotting (cluster 10 would default to come after cluster 1 and before cluster 2)
km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8'))


km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point(color="#232D4B") + # color points
  gghighlight(Cluster=='Cluster 1', use_direct_label = FALSE) + # highlight cluster 1
  labs(x = "Predictor", y = "Cluster Center",  # axis labels
       title = "Visualizing K-Means Cluster Makeups", # plot title
       subtitle = "Cluster 1") +  # plot subtitle
  theme_minimal() +# add themes
  theme(legend.position = "none", # manually adjust themes
        axis.text.x = element_text(angle=45, size=10))

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Visualizing K-Means Cluster Makeups") + 
  theme_minimal() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank())



pca <- prcomp(min_scale) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

# plot % of variance between players explained by each subsequent PC 
tibble(imp = pca_summary$importance[2,], n = 1:length(imp)) %>% # get importance scores for PCA summary
  ggplot(aes(x=n, y=imp)) + 
  labs(x = 'Principle Component #', y = '% of Variance Explained by Component',
       title = 'Less Information is Gained by Each Subsequent PC') +
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  theme_minimal() + scale_x_continuous(breaks=seq(1, 20, 1)) + # set x-axis
  scale_y_continuous(labels=scales::percent) + # change y-axis from proportion to percentage
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans8$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2


pc2 %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + stat_ellipse(level=(2/3)) + # set ellipse value to one standard deviation
  scale_shape_manual(values=seq(0,15)) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'Visualizing K-Means Cluster Differences in 2D')

