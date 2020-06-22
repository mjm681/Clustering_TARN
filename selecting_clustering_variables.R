##### Selecting Variables for Clustering #####

library(stats)

full_data <- read.csv("processed_filtered_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1)

column_names <- c('ASSESS_SYSBP_VAL', 'ASSESS_CREFILL_NORM', 'ASSESS_GCS_MOTOR', 
                  'ASSESS_GCS_TOTAL', 'ASSESS_PULSE_VAL', 'ASSESS_RESP_RATE_VAL', 
                  'INJURY_MECHANISM', 'INJURY_TYPE', 'PATIENT_AGE', 'PATIENT_GENDER', 
                  'CAPTIME_REVISED_IsMoreThan2', 'Head', 'Face', 'Chest', 'Abdomen', 
                  'Pelvis', 'Spine', 'Limb', 'Other', 'Has_Lerner_LSI_5_Lerner_time',
                  'P1_T_or_F')

cols <- c(17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 86)

full_data_na <- full_data[complete.cases(full_data),]
cluster_df <- full_data_na[,cols]
rownames(cluster_df) <- cluster_df$caseid
cluster_df <- cluster_df[,-1]
write.csv(cluster_df, "complete_cluster_df.csv")

cluster_df <- full_data[,cols]
rownames(cluster_df) <- cluster_df$caseid
cluster_df <- cluster_df[,-1]
write.csv(cluster_df, "incomplete_cluster_df.csv")

