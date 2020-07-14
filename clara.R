##### Setting Up Environment #####

set.seed(123)

library(caret)
library(ggplot2)
library(fpc)
library(factoextra)
library(cluster)
library(dplyr)
library(Rtsne)
library(NbClust)
library(reshape)

##### Loading Data #####

clara_data <- read.csv("final_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1)

cluster_df_na <- clara_data[complete.cases(clara_data),] # Exclude any cases with any missing data

##### One-Hot Encoding the Data #####

dummy_vars <- dummyVars("~ ." , cluster_df_na, fullRank = F)

dum_df <- data.frame(predict(dummy_vars, newdata = cluster_df_na))



most_k_ind <- 3 # Output from NbClust k-optimisation estimation

##### Running CLARA #####

clara_res <- clara(dum_df,
                   k = most_k_ind, # Use optimum k from NbClust indices
                   metric = 'euclidean',
                   stand = T,
                   samples = 100,
                   sampsize = 40000,
                   pamLike = T,
                   correct.d = T,
                   keep.data = T)

cluster_data <- cbind(dum_df, cluster = clara_res$cluster)

medoids <- clara_res$medoids

##### Summary of Clusters #####

clara_results <- dum_df %>%
  dplyr::select() %>%
  mutate(cluster = clara_res$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

clara_summary <- as.data.frame(clara_results$the_summary)

clustering_vector <- as.data.frame(clara_res$clustering)

dum_data_clusters <- cbind(dum_df, cluster = clara_res$clustering)

data_clusters <- cbind(cluster_df_na, cluster = clara_res$clustering)

##### Visualisation using t-SNE #####

# Run t-SNE
tsne_obj <- Rtsne(dum_df, is_distance = F, check_duplicates = F)

# Manipulate the results of t-SNE into format that can be plotted easier 
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clara_res$clustering),
         name = dum_df$P1_T_or_FTRUE)

# Generate pdf of t-SNE plot
pdf("tsne_plot.pdf")
tsne_plot <- ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
dev.off()

##### Silhouette info for CLARA result #####

# Make pdf of the silhouette across the clusters
pdf("sil_plot.pdf")
fviz_silhouette(clara_res)
dev.off()

# Silhouette information
silinfo <- clara_res$silinfo
names(silinfo)

# Silhouette widths of each observation
head(silinfo$widths[, 1:3], 10)

# Average silhouette width of each cluster
silinfo$clus.avg.widths

# The total average (mean of all individual silhouette widths)
silinfo$avg.width

# The size of each clusters
clara_res$size

# Silhouette width of observation
sil <- clara_res$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

##### Saving #####

now <- Sys.time() # Make unique save from the time and date
file_name <- paste("clara_", format(now, "%Y%m%d_%H%M%S_"), sep = "")
save(list = ls(), file = file_name)
