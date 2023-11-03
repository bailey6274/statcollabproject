min <- read_csv("minnesota_data.csv")

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





set.seed(2002) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 



MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(min, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

# color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")




tibble(k = 1:MAX_K, SSE = sse) %>%
  ggplot(aes(x=k, y=SSE)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + # set color of point and lines
  labs(x = "K", y = "SSE", title = "SEE Across K-Clusters") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal() + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) + 
  scale_colour_manual(values=cbPalette)# manually alter theme

tibble(k = 1:MAX_K, SSE_difference = sse-lead(sse)) %>%
  dplyr::filter(k<MAX_K) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + # set color of point and lines
  labs(x = "K", y = "SSE Rolling Difference", title = "A Clearer Picture") + # set axis/plot titles
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + # define x-axis
  theme_minimal()  + # add themes
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) # manually alter theme

k.diff1 <- data.frame(k = 1:MAX_K, SSE_difference = sse-lead(sse)) %>%
  dplyr::filter(k<MAX_K-1)
k.diff2 <- data.frame(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1)

ggplot(data = k.diff2, aes(x=k, y=SSE_difference)) + 
  geom_point(color="#56B4E9") + 
  geom_line(color="#999999") + 
  geom_point(aes(x = 9, y = SSE_difference[9]), shape = 1, size = 5, alpha = .7, color = "#E69F00") +
  labs(x = "K", y = "SSE", title = "Two-Unit Rolling Distance of SSE Across K-Clusters") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() +  
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())


K <- 9

k.m <- kmeans(min, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(k.m$centers) # SCALED cluster centers/means

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6',
                        'Cluster 7', 'Cluster 8', 'Cluster 9') 
# massage data
km_centers <- km_centers %>%
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier

# reset the order of predictor variables for plotting
km_centers$feature <- factor(km_centers$feature, levels=c("HEIGHT","WEIGHT","FG_PCT","FG3_PCT","FT_PCT","REB","AST","TOV","PTS","STL","AGE","BLK")) 

# reset the order of clusters for plotting (cluster 10 would default to come after cluster 1 and before cluster 2)
km_centers$Cluster <- factor(km_centers$Cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6', 'Cluster 7', 'Cluster 8', 'Cluster 9'))


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



pca <- prcomp(min) # perform Principle Component Analysis 
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
pc2$Cluster <- as.factor(k.m$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

pc2 %>% 
  ggplot(aes(x=PC1, y=PC2, color=Cluster, shape=Cluster)) + 
  geom_point(alpha=0.3) + 
  geom_rug() + # great way to visualize points on a single axis
  theme_minimal() + 
  stat_ellipse(level=(2/3)) + # set ellipse value to one standard deviation
  scale_shape_manual(values=seq(0,15)) + 
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'K-Means Cluster Differences Across First Two Principle Components')

