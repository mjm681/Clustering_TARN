library(caret)
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

##### One hot encoding #####

dummy_vars <- dummyVars("~ ." , cluster_df_na, fullRank = F)
# Need to remove colinear factors where the factor level = 2 (binary) because they are either 1 or 0, dummy trap 
# full.Rank = T

dum_df <- data.frame(predict(dummy_vars, newdata = cluster_df_na))

##### Optimising k #####

small_dum_df <- as.matrix(dum_df[1:50000,]) # To test code

# Metrics to be used
metrics <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette","gamma",  
             "ball", "ptbiserial", "gap", "frey", "mcclain", 
             "gplus",  "dunn", "sdbw") # Metrics used
graph_metrics <- c("hubert", "sdindex") # Not used
not_work <- c("ccc", "scott", "marriot", "trcovw", "tracew", "friedman", "rubin",
              "ratkowsky", "sdindex", "duda","pseudot2", "beale", "tau") # Not used

# Distances to be used
distances <- c("Metric","euclidean", "maximum", "manhattan", "canberra", "minkowski")

# Making the dataframe for results
tabla <- as.data.frame(matrix(ncol = length(distances), nrow = length(metrics)))
names(tabla) <- distances

# Using NbClust for all the distances and metrics and filling in the dataframe
for (j in 2:length(distances)){
  for(i in 1:length(metrics)){
    nb = NbClust(small_dum_df, distance = distances[j],
                 min.nc = 2, max.nc = 20, 
                 method = "ward.D", index = metrics[i])
    tabla[i,j] = nb$Best.nc[1]
    tabla[i,1] = metrics[i]
  }
}

write.csv(tabla, "opt_k.csv") # Save results of the optimisation

### Plot Optimum K ###

melt_tabla <- melt(tabla, id = "Metric")
melt_tabla$value <- as.factor(melt_tabla$value)

for_plot <- melt_tabla[,-1]
count_data <- as.data.frame(table(for_plot))

plot_opt_k <- ggplot(count_data, aes(x = value, y = Freq ,fill = variable, group = variable)) +
  geom_bar(position = "stack",  stat = "identity") +
  labs(title = "Estimating Optimum Number of Clusters")

sum_freq <- as.data.frame(table(for_plot$value))

most_k_ind <- as.numeric(which.max(sum_freq$Freq)) # Save the most reported value of k 

##### Saving #####

file_name <- paste("opt_k", format(now, "%Y%m%d_%H%M%S_"), sep = "")
save(list = ls(), file = file_name)

