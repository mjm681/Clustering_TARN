##### Setting up environment #####

library(dplyr)
library(Amelia)
library(plyr)
library(ggplot2)
library(forcats)

# Load raw data
raw_data <- read.csv("Physio_withGOS_AIS_England_Wales_FinalVersion.csv", header = T, check.names = F, na.strings = "", row.names = 1)
raw_data <- raw_data[,-1]

# Load processed data
final_data <- read.csv("final_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1)

# Load included processed data
column_names <- c('caseid', 'ASSESS_SYSBP_VAL', 'ASSESS_CREFILL_NORM', 'ASSESS_GCS_MOTOR', 
                  'ASSESS_GCS_TOTAL', 'ASSESS_PULSE_VAL', 'ASSESS_RESP_RATE_VAL', 
                  'INJURY_MECHANISM', 'INJURY_TYPE', 'PATIENT_AGE', 'PATIENT_GENDER', 
                  'CAPTIME_REVISED_IsMoreThan2', 'Head', 'Face', 'Chest', 'Abdomen', 
                  'Pelvis', 'Spine', 'Limb', 'Other', 'Has_Lerner_LSI_5_Lerner_time',
                  'P1_T_or_F')

cols <- c(1, 17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 86)

cluster_df <- final_data[,cols]
rownames(cluster_df) <- cluster_df$caseid
cluster_df <- cluster_df[,-1]

cluster_data <- cluster_df

# Variable to change for plotting missingness
full_data <- raw_data

##### Investigate Missingness #####

missings <- as.data.frame(colSums(is.na(full_data)))
missings$Percentage_missing <- (missings$`colSums(is.na(full_data))`)/nrow(full_data)*100
colnames(missings) <- c("Num_missing", "Percentage_missing")
#missmap(full_data)

ggplot(missings, aes(x = row.names(missings), y = Percentage_missing)) +
  geom_bar(stat = 'identity')

sorted_missings <- missings[order(missings$Percentage_missing),]
sorted_missings$index <- c(1:21) # or 86 or 21
ggplot(sorted_missings, aes(x = index, y = Percentage_missing)) +
  geom_point() +
  labs(title = "Missingness per Variable in Reduced Data for Clustering")

##### Make NA explicit for Factor by Factor Investigation #####

# Vector of index of categorical variables for raw and pre-processed data respectively
cat_var_raw <- c(3,6:8,13:15,18:19,23,25:28,32:38,40,41:47,49:50,51:53,55:61,72:74,76:77,79)
cat_var_final <- c(3,6:8,13:15,18:19,23,25:28,32:38,40,41:47,49:50,51:53,55:61,72:74,76:77,79:86)
cat_var_cluster <- c(2:3,8:9,11,20:21)

cat_var <- cat_var_raw

for (i in 1:length(cat_var)) {
  var <- cat_var[i]
  full_data[[var]] <- as.factor(full_data[[var]])
  full_data[[var]] <- fct_explicit_na(full_data[[var]])
}

##### Missingness for continuous columns #####

# Create function to add new factor levels
addLevel <- function(x, newlevel=NULL){ 
  if(is.factor(x)) {
    return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

count_na <- data.frame(Column = character(), Freq_NA = integer())
count_na[(nrow(count_na)+1):(nrow(count_na)+31),] <- NA

num_var_all <- c(2,4,5,9:12,16,17,20:22,24,29:31,39,48,54,62:71,75,78)
num_var_clu <- c(1,4:7,10,12:19)

num_var <- num_var_all

for (i in 1:length(num_var)) {
  var <- num_var[[i]]
  nam <- colnames(full_data[num_var[i]])
  count_na$Column <- addLevel(count_na$Column, nam)
  count_na[i,1] <- colnames(full_data[var])
  count_na[i,2] <- sum(is.na(full_data[var]))
}

write.csv(count_na, "numeric_missing.csv")

##### Variable Types #####

summary <- summary(full_data)
write.csv(summary, "summary.csv")

##### Saving Variable Levels #####

for (i  in 1:ncol(full_data)) {
  factor <- as.data.frame(table(full_data[i]))
  factor_nam <- colnames(full_data[i])
  if ( colnames(full_data[i]) == "SEDATION/ANAESTHESIA" ) {
    factor_nam <- "SEDATION-ANAESTHESIA"
  }
  if ( colnames(full_data[i]) == "INOTROPES/VASOSUPRESSORS" ) {
    factor_nam <- "INOTROPES-VASOSUPRESSORS"
  }
  if ( colnames(full_data[i]) == "Recieved Airway/Breathing Support" ) {
    factor_nam <- "Recieved_Airway_Breathing_Support"
  }
  nam <- paste(factor_nam, "_factors.csv")
  write.csv(x = factor, file = nam)
}

##### Saving Overall #####

save(list = ls(), file = "missingness")
