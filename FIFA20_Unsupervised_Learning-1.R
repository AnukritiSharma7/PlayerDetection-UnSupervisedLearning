library (tidyverse) 
library (ggfortify)
library (qdapTools)
library (tidyr)
library (corrplot)
library (ggpubr)
library(ggplot2)
library(PerformanceAnalytics)
library(factoextra)
library(fpc)
install.packages("DataExplorer")
library(DataExplorer)

install.packages("dplyr")
install.packages("tidyr")
install.packages("GGally")
install.packages("Lahman")

setwd("C:/Users/prana/OneDrive - University of Cincinnati/Desktop/DMBI/FIFA")
fifa_data <- read.csv("players_20.csv", stringsAsFactors = FALSE)


## Analyzing the data

plot_str(fifa_data)
plot_missing(fifa_data)
plot_density(fifa_data)
plot_correlation(fifa_data, type = 'continuous','Review.Date')
plot_bar(fifa_data)

create_report(fifa_data)


# Dividing the Data into Goalkeepers and outfield players

#Goalkeeper data:

data_gk <- subset(x=fifa_data, subset= player_positions == "GK", select = c(sofifa_id,short_name,nationality,overall,potential,player_positions,gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning,power_shot_power,power_stamina))


create_report(data_gk)

#	Hierarchical clustering

distances = dist(data_gk[3:14],method = "euclidean")
cluster_gk = hclust(distances,method = "ward")

# Colored Dindogram

fviz_dend(cluster_gk,k = 3,
                    cex = 0.5, # label size
                    k_colors = c( "#2E9FDF", "#00AFBB", "#E7B800" ),
                    color_labels_by_k = TRUE, # color labels by groups
                    rect = TRUE, # Add rectangle around groups
                    rect_border = c("#00AFBB", "#E7B800"), 
                    rect_fill = TRUE)


# Group

clustergroups = cutree(cluster_gk,k=3)

tapply(data_gk$gk_reflexes,clustergroups, mean)
tapply(data_gk$gk_speed,clustergroups, mean)
tapply(data_gk$overall,clustergroups, mean)
tapply(data_gk$potential,clustergroups, mean)
tapply(data_gk$gk_diving,clustergroups, mean)
tapply(data_gk$gk_handling,clustergroups, mean)
tapply(data_gk$gk_kicking,clustergroups, mean)
tapply(data_gk$gk_positioning,clustergroups, mean)
tapply(data_gk$power_shot_power,clustergroups, mean)
tapply(data_gk$power_stamina,clustergroups, mean)


# Checking the player replacement

subset(data_gk, short_name == "David De Gea Quintana")
clustergroups[15]

data_gk[clustergroups==1,]

cluster2  = subset(data_gk, clustergroups == 1)
cluster2$short_name[1:10]

# Agreement between species and HC clusters
res.hc <- eclust(data_gk_scale, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
cluster.stats(d = dist(new_df_maingk.scaled), 
              species, res.hc$cluster)$vi

clust_stats_hcl <- cluster.stats(d = dist(data_gk_scale), 
                             res.hc$cluster)


# Outfield Players


data_outfield <- subset(x=fifa_data, subset= player_positions != "GK", select = c(sofifa_id,short_name,nationality,overall,potential,player_positions,pace,shooting,passing,dribbling,defending,physic,attacking,power_shot_power,power_stamina))
create_report(data_outfield)

# Removing nominal data

data_out_removed <- subset(data_outfield,select = -c(short_name, player_positions, sofifa_id,nationality))
sil_hl <- silhouette(cluster_gk$order,dist(data_out_removed))
fviz_silhouette(sil_hl)

# Clustering

distances_outfield = dist(data_outfield[3:15],method = "euclidean")
cluster_outfield = hclust(distances_outfield,method = "ward")

clustergroups_outfiled = cutree(cluster_outfield,k=3)


tapply(data_outfield$overall,clustergroups_outfiled, mean)
tapply(data_outfield$potential,clustergroups_outfiled, mean)
tapply(data_outfield$pace,clustergroups_outfiled, mean)
tapply(data_outfield$shooting,clustergroups_outfiled, mean)
tapply(data_outfield$passing,clustergroups_outfiled, mean)
tapply(data_outfield$dribbling,clustergroups_outfiled, mean)
tapply(data_outfield$defending,clustergroups_outfiled, mean)
tapply(data_outfield$physic,clustergroups_outfiled, mean)
tapply(data_outfield$attacking,clustergroups_outfiled, mean)
tapply(data_outfield$power_shot_power,clustergroups_outfiled, mean)
tapply(data_outfield$power_stamina,clustergroups_outfiled, mean)

# Checking the replacment for a player

subset(data_outfield, short_name == "Harry Kane")

clustergroups_outfiled[13]

cluster2  = subset(data_outfield, clustergroups_outfiled == 2)
cluster2$short_name[1:10]


# dendrogram 
fviz_dend(cluster_outfield,k = 3,
          cex = 0.5, # label size
          k_colors = c( "#2E9FDF", "#00AFBB", "#E7B800" ),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#00AFBB", "#E7B800"), 
          rect_fill = TRUE)


plot(cluster_outfield)



#######################################################

# K-means 
# Scaling data

data_gk_scale <- subset(data_gk,select = -c(short_name, player_positions, sofifa_id,nationality))
data_gk_scale <- scale(data_gk_scale)


new_df <- data_gk_scale[-c(150:nrow(data_gk_scale)), ]
gk_50 <- data_gk[-c(150:nrow(data_gk)), ]


#determine and visualize optimal number of cluster 
install.packages("factoextra")
library(factoextra)



#k means elbow curve
fviz_nbclust(data_gk_scale, kmeans , method = "wss")


# K means
set.seed(1234)
fit.km <- kmeans(data_gk_scale, 3, nstart=25)  


#Cluster plot
fit.km <- kmeans(new_df, 3, nstart=25) 
str(new_df1)
str(data_gk)
new_df1<- data.frame(new_df)
new_df2 <- cbind(new_df1, short_name = gk_50$short_name)
row.names(new_df2) <- new_df2$short_name
new_df2 <- new_df2[-1]
new_df3 <- subset(new_df2,select = -c(short_name))

fviz_cluster(fit.km, data = new_df3)
print(fit.km)

km.res <- eclust(data_gk_scale,"kmeans", k = 3 , nstart=25, graph = FALSE)

# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
               palette = "jco", ggtheme = theme_minimal())


# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

table(data_gk$short_name, km.res$cluster)

# Compute cluster stats
gk2 <- as.numeric(new_df_maingk$sofifa_id)

clust_stats <- cluster.stats(d = dist(data_gk_scale), 
                           km.res$cluster)
# Corrected Rand index
clust_stats$corrected.rand

df1_complete <- na.omit(data_gk_scale)

complete3 <- cutree(kmeans(new_df),3)



### outfield k means


# Data scaling
data_outfield_scale <- subset(data_outfield,select = -c(short_name, player_positions, sofifa_id,nationality))
data_outfield_scale <- scale(data_outfield_scale)


# K means

fviz_nbclust(data_outfield_scale, kmeans , method = "wss")
set.seed(1234)

fit.km <- kmeans(data_outfield_scale, 3, nstart=25)  

print(fit.km)

# Cluster plot
outfield_50 <- data_gk[-c(150:nrow(data_gk)), ]

new_df_outfield <- data_outfield_scale[-c(150:nrow(data_outfield_scale)), ]
fit.km <- kmeans(new_df_outfield, 3, nstart=25)

new_df1_outfield<- data.frame(new_df_outfield)
new_df2_outfield <- cbind(new_df1_outfield, short_name = outfield_50$short_name)
row.names(new_df2_outfield) <- new_df2_outfield$short_name
new_df2_outfield <- new_df2_outfield[-1]
new_df3_outfield <- subset(new_df2_outfield,select = -c(short_name))

fviz_cluster(fit.km, data = new_df3_outfield)
