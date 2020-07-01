##### Setting Up Environment #####

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

column_names <- c('caseid', 'ASSESS_SYSBP_VAL', 'ASSESS_CREFILL_NORM', 'ASSESS_GCS_MOTOR', 
                  'ASSESS_GCS_TOTAL', 'ASSESS_PULSE_VAL', 'ASSESS_RESP_RATE_VAL', 
                  'INJURY_MECHANISM', 'INJURY_TYPE', 'PATIENT_AGE', 'PATIENT_GENDER', 
                  'CAPTIME_REVISED_IsMoreThan2', 'Head', 'Face', 'Chest', 'Abdomen', 
                  'Pelvis', 'Spine', 'Limb', 'Other', 'Has_Lerner_LSI_5_Lerner_time',
                  'P1_T_or_F')

cols <- c(1, 17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 86)

cluster_df <- clara_data[,cols] # Selecting variables for clustering
rownames(cluster_df) <- cluster_df$caseid # Assingning caseid to rownames 
cluster_df <- cluster_df[,-1]

cluster_df_na <- cluster_df[complete.cases(cluster_df),] # Exclude any cases with any missing data

##### One-Hot Encoding the Data #####

dummy_vars <- dummyVars("~ ." , cluster_df_na, fullRank = F)

dum_df <- data.frame(predict(dummy_vars, newdata = cluster_df_na))

set.seed(123)

##### NbClust for k-optimisation #####

k_test <- 20 # Number of clusters to try

metrics <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette","gamma",  
             "ball", "ptbiserial", "gap", "frey", "mcclain", 
             "gplus",  "dunn", "sdbw") # Metrics used
graph_metrics <- c("hubert", "sdindex") # Not used
not_work <- c("ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin",
              "ratkowsky", "sdindex", "duda","pseudot2", "beale", "tau") # Not used
distances <- c("Metric","euclidean", "maximum", "manhattan", "canberra", "minkowski") # Distances used

# Making an empty dataframe for results
tabla <- as.data.frame(matrix(ncol = length(distances), nrow = length(metrics)))
names(tabla) <- distances

# Running NbClust k-optimisation on all the distances and metrics and saving the result in the dataframe
for (j in 2:length(distances)){
  for(i in 1:length(metrics)){
    
    nb = NbClust(dum_df, distance = distances[j],
                 min.nc = 2, max.nc = k_test, 
                 method = "ward.D", index = metrics[i])
    tabla[i,j] = nb$Best.nc[1]
    tabla[i,1] = metrics[i]
    
  }}

write.csv(tabla, "opt_k.csv") # Save the result of the optimisation

### Plotting k-optimising ###

melt_tabla <- melt(tabla, id = "Metric")
melt_tabla$value <- as.factor(melt_tabla$value)

for_plot <- melt_tabla[,-1]
count_data <- as.data.frame(table(for_plot))

plot_opt_k <- ggplot(count_data, aes(x = value, y = Freq ,fill = variable, group = variable)) +
  geom_bar(position = "stack",  stat = "identity") +
  labs(title = "Estimating Optimum Number of Clusters")


sum_freq <- as.data.frame(table(for_plot$value))

most_k_ind <- as.numeric(which.max(sum_freq$Freq)) # Save the most reported value of k 

##### Silhouette Width Plot #####

sil_width <- c(NA)

for(i in 2:k_test){
  
  clara_fit <- clara(dum_df,
                     k = i,
                     metric = 'euclidean',
                     stand = T,
                     samples = 100,
                     sampsize = 15000,
                     pamLike = T,
                     correct.d = T)
  sil_width[i] <- clara_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)
pdf("silhouette width.pdf")
sil_plot <- plot(1:k_test, sil_width,
                 xlab = "Number of clusters",
                 ylab = "Silhouette Width")
sil_plot <- sil_plot + lines(1:k_test, sil_width)
dev.off()

opt_k <- which.max(sil_width)

##### Running CLARA #####

clara_res <- clara(dum_df,
                   k = most_k_ind, # Use optimum k from NbClust indices
                   metric = 'euclidean',
                   stand = T,
                   samples = 100,
                   sampsize = 15000,
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

##### Visualisation #####

tsne_obj <- Rtsne(dum_df, is_distance = F, check_duplicates = F)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(clara_res$clustering),
         name = dum_df$P1_T_or_FTRUE)

pdf("tsne_plot.pdf")
tsne_plot <- ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))
dev.off()


##### Silhouette info for CLARA result #####

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

now <- Sys.time()
file_name <- paste("clara_", format(now, "%Y%m%d_%H%M%S_"), sep = "")
save(list = ls(), file = file_name)
