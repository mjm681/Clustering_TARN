##### Setting up Environment ##### 

library(ggplot2)
library(stringr)
library(purrr)
library(tidyr)
library(data.table)

### Loading data ###

test_data <- read.csv("final_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1) # Data to be plotted

load("clara_results") # CLARA or PAM 

##### Plotting Categorical Variables #####

# Function to plot categorical variables as frequency plot
cat_freq <- function(data, variable, cluster = F) {
  variable <- dplyr::enquo(variable)
  if (isTRUE(cluster)) {
    title_paste <- substitute(paste("Plotting Categoric Variable per Cluster:", variable))
  }
  else {
    title_paste <- paste("Frequency of ", quo_name(variable), " Factor Levels in Data")
  }
  plotting <- ggplot(data = data, aes(x = !!variable, fill = !!variable)) +
    geom_bar(stat = "count", width = 1) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
    labs(title = title_paste)
  return(plotting)
}

# Calling the function
cat_freq(full_data, variable =  Has_Lerner_LSI_5_Lerner_time, cluster = F)

##### Plotting Continuous Variables #####

# Function to plot continuous variables as histogram
cont_hist <- function(data, variable, cluster = F) {
  variable <- dplyr::enquo(variable)
  if (isTRUE(cluster)) {
    title_paste <- substitute(paste("Plotting Variable per Cluster:", variable))
  }
  else {
    title_paste <- paste("Histogram of ", quo_name(variable))
  }
  plotting <- ggplot(data = data, aes(x = !!variable, fill = !!variable)) +
    geom_histogram(colour = "black", fill = "lightblue") +
    labs(title = title_paste)
  return(plotting)
}

# Calling the function
cont_hist(full_data, variable = PATIENT_AGE, cluster = F)

### Finding Peaks of Histogram ##

max_ind <- as.numeric(which.max(density(full_data$ASSESS_RESP_RATE_VAL)$y))

density(full_data$ASSESS_RESP_RATE_VAL)$x[max_ind]

## Finding second peak

max_y <- max(density(full_data$PATIENT_AGE)$y[density(full_data$PATIENT_AGE)$x < 65])

max_ind_2 <- which(density(full_data$PATIENT_AGE)$y == max_y)

density(full_data$PATIENT_AGE)$x[max_ind_2]

## Finding 3rd peak

max_y <- max(density(full_data$PATIENT_AGE)$y[density(full_data$PATIENT_AGE)$x < 40])

max_ind_3 <- which(density(full_data$PATIENT_AGE)$y == max_y)

density(full_data$PATIENT_AGE)$x[max_ind_3]

##### Plotting Injruy Scores (AIS, ISS and GCS) ##### 

injury_scores <- full_data[,12:19] # Select AIS scores from 21 varible clustering dataset

injury_scores$ISS <- NA

# Calculating Injury Severity Score from constituents
for (i in 1:nrow(injury_scores)) {
  x <- c(injury_scores[i,1],injury_scores[i,2],injury_scores[i,3],
         injury_scores[i,4],injury_scores[i,5],injury_scores[i,6],
         injury_scores[i,7],injury_scores[i,8])
  n <- as.numeric(length(x))
  A <- sort(x, partial = n)[n] # Finding largest score
  B <- sort(x, partial = n-1)[n-1] # Finding second largest score
  C <- sort(x, partial = n-2)[n-2] # Finding third largest score
  iss_calc <- sum((A^2), (B^2), (C^2)) # Combine square of top 3 scores for ISS total
  if ( 6 %in% x == TRUE ) { # If any score is 6 (max), ISS is assigneed 75 
    iss_calc <- 75
  }
  injury_scores[i,9] <- iss_calc
}

# Make faceted plot of all AIS scores
injury_scores %>%
  keep(is.numeric) %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free_x") +
  geom_histogram(stat = "count", colour = "black", fill = "lightblue") +
  labs(title = "Frequency of AIS and ISS") 

# Plot GCS scores
ggplot(full_data, aes(x = ASSESS_GCS_TOTAL)) +
  geom_bar(stat = "count", fill = "lightblue", colour = "black") +
  labs(title = "Frequency of GCS Total") 

##### Splitting PAM/CLARA object into summary lists to facilitate plotting #####

# Split data into cluster summaries
for ( k in 1:opt_k ) {
  cluster_investigation <- list()
  data_of_cluster <- subset(data_clusters, data_clusters$cluster == k)
  for (c in 1:ncol(data_of_cluster)) {
    if ( isTRUE(is.numeric(data_of_cluster[[c]])) ) {
      df <- data.frame(unclass(summary(data_of_cluster[[c]])), check.names = F, stringsAsFactors = F)
    } else {
      df <- as.data.frame(table(data_of_cluster[c]))
    }
    if ( nrow(as.data.frame(table(data_of_cluster[c]))) == 0 ) {
      df <- data.frame(Var1 = character(),
                       Freq = integer())
    }
    cluster_investigation <- c(cluster_investigation, list(df))
  }
  names(cluster_investigation) <- colnames(data_of_cluster)
  nam <- paste("Cluster", k, "has", nrow(data_of_cluster), "cases", sep = "")
  assign(nam, cluster_investigation)
}

# Make list of data in sample
for ( k in 1 ) {
  cluster_investigation <- list()
  data_of_cluster <- as.data.frame(data_clusters)
  for (c in 1:ncol(data_of_cluster)) {
    if ( isTRUE(is.numeric(data_of_cluster[[c]])) ) {
      df <- data.frame(unclass(summary(data_of_cluster[[c]])), check.names = F, stringsAsFactors = F)
    } else {
      df <- as.data.frame(table(data_of_cluster[c]))
    }
    if ( nrow(as.data.frame(table(data_of_cluster[c]))) == 0 ) {
      df <- data.frame(Var1 = character(),
                       Freq = integer())
    }
    cluster_investigation <- c(cluster_investigation, list(df))
  }
  names(cluster_investigation) <- colnames(data_of_cluster)
  nam <- paste("total_data")
  assign(nam, cluster_investigation)
}

# Make list of full dataset
for ( k in 1 ) {
  cluster_investigation <- list()
  data_of_cluster <- as.data.frame(cluster_df)
  for (c in 1:ncol(data_of_cluster)) {
    if ( isTRUE(is.numeric(data_of_cluster[[c]])) ) {
      df <- data.frame(unclass(summary(data_of_cluster[[c]])), check.names = F, stringsAsFactors = F)
    } else {
      df <- as.data.frame(table(data_of_cluster[c]))
    }
    if ( nrow(as.data.frame(table(data_of_cluster[c]))) == 0 ) {
      df <- data.frame(Var1 = character(),
                       Freq = integer())
    }
    cluster_investigation <- c(cluster_investigation, list(df))
  }
  names(cluster_investigation) <- colnames(data_of_cluster)
  nam <- paste("full_data")
  assign(nam, cluster_investigation)
}


##### Split data into clusters #####

for ( k in 1:nrow(clara_res$medoids) ) {
  data_of_cluster <- subset(data_clusters, cluster == k)
  nam <- paste("Cluster", k, "data", sep = "")
  assign(nam, data_of_cluster)
}

##### Assessing Silhouette Width of Clusters #####

widths <- as.data.frame(pam_fit$silinfo$widths)

# Plot the average silhouette width per cluster and draw a line at the average silhouette width 
dev.new(width = 5, height = 4, noRStudioGD = TRUE)
ggplot(as.data.frame(pam_fit$silinfo$clus.avg.widths), aes(x = row.names(as.data.frame(pam_fit$silinfo$clus.avg.widths)), y = pam_fit$silinfo$clus.avg.widths)) +
  geom_point(size = 3) +
  geom_hline( yintercept = pam_fit$silinfo$avg.width ) +
  labs(title = "Average Silhouette Width per Cluster for Sample 2") + 
  xlab("Cluster Number") +
  ylab("Average Silhouette Width")

# Plot the ASW calculation for k-optimisation calculation where appropriate
dev.new(width = 5, height = 4, noRStudioGD = TRUE)
sil_plot <- plot(1:k_test, sil_width,
                 xlab = "Number of clusters",
                 ylab = "Silhouette Width",
                 main = "Sample 2 \nAverage Silhouette Width in k-Optimisation"
)
sil_plot + lines(1:k_test, sil_width)
dev.off()

##### Plotting Cluster Number vs Average Silhouette Width #####

points <- asw_points # CSV of the data

#points$Sample <- as.factor(points$Sample)
points$`Cluster Number` <- as.factor(points$`Cluster Number`)


ggplot(points, aes(x = Points, y = `Average Silhouette Width`, colour = `Cluster Number`, shape = `Cluster Number`)) +
  geom_point(size = 5) +
  ggtitle("Number of Points per Cluster compared to Average Silhouette Width of those Points for PAM Sampling")


##### Plotting the cluster data structure per variable #####

### Numeric Variables ###

# Make dataframe of the variable to use in the plotting, changes depending on the number of clusters
merge <- as.data.frame(cbind(Cluster1has3695cases$ASSESS_SYSBP_VAL, 
                             Cluster2has3704cases$ASSESS_SYSBP_VAL, 
                             Cluster3has3236cases$ASSESS_SYSBP_VAL, 
                             Cluster4has3058cases$ASSESS_SYSBP_VAL, 
                             Cluster5has1307cases$ASSESS_SYSBP_VAL,
                             total_data$ASSESS_SYSBP_VAL, # Remove this line when investigating CLARA
                             full_data$ASSESS_SYSBP_VAL))
colnames(merge) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Full Data", "Sample Data", "Full Data")
merge <- as.data.frame(t(merge))
merge$Data <- rownames(merge)

# Make the plot
dev.new(width = 8, height = 4, noRStudioGD = TRUE)
ggplot(merge, aes(x = Data, fill = Data )) +
  geom_boxplot( aes(ymin = `Min.`, lower = `1st Qu.`, middle = `Median`, upper = `3rd Qu.`, ymax = `Max.`),
                stat = "identity") +
  ylab("ASSESS_SYSBP_VAL") +
  labs(title = "Sample 1")

### Categorical Variables ###

# Make dataframe of the variable to use in the plotting, changes depending on the number of clusters
merge <- as.data.frame(cbind(Cluster1has3695cases$PATIENT_GENDER$Freq, 
                             Cluster2has3704cases$PATIENT_GENDER$Freq, 
                             Cluster3has3236cases$PATIENT_GENDER$Freq, 
                             Cluster4has3058cases$PATIENT_GENDER$Freq, 
                             Cluster5has1307cases$PATIENT_GENDER$Freq,
                             total_data$PATIENT_GENDER$Freq, full_data$PATIENT_GENDER$Freq ))
colnames(merge) <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Sample Data", "Full Data")
merge$Factor <- full_data$PATIENT_GENDER$Var1

if (merge[1,1] %in% "logical") {
  merge <- merge[-1,]
  melt_merge <- melt(merge, id = c("Factor"))
} else {
  melt_merge <- melt(merge, id = c("Factor"))
}

melt_merge$Factor <- as.factor(melt_merge$Factor)
melt_merge$variable <- as.factor(melt_merge$variable)
melt_merge$value <- as.numeric(melt_merge$value)

# Make the plot
dev.new(width = 8, height = 4, noRStudioGD = TRUE)
ggplot(melt_merge, aes(x = variable, y = value, fill = Factor)) +
  geom_bar( position = "fill", stat = "identity", colour = "black") +
  ylab("PATIENT_GENDER") +
  xlab("Data") +
  labs(title = "Sample 2 \nMost Important Feature", fill = "PATIENT_GENDER")
