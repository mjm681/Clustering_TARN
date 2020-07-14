##### Code for BlueBEAR #####

library(Gifi)
library(ggplot2)

clara_data <- read.csv("final_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1)

column_names <- c('caseid', 'ASSESS_SYSBP_VAL', 'ASSESS_CREFILL_NORM', 'ASSESS_GCS_MOTOR', 
                  'ASSESS_GCS_TOTAL', 'ASSESS_PULSE_VAL', 'ASSESS_RESP_RATE_VAL', 
                  'INJURY_MECHANISM', 'INJURY_TYPE', 'PATIENT_AGE', 'PATIENT_GENDER', 
                  'CAPTIME_REVISED_IsMoreThan2', 'Head', 'Face', 'Chest', 'Abdomen', 
                  'Pelvis', 'Spine', 'Limb', 'Other', 'Has_Lerner_LSI_5_Lerner_time',
                  'P1_T_or_F')

cols <- c(1, 17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 86)

cluster_df <- clara_data[,cols]
rownames(cluster_df) <- cluster_df$caseid
cluster_df <- cluster_df[,-1]

cluster_df_na <- cluster_df[complete.cases(cluster_df),]

##### Performing PCA #####

ordinals = c(T, F, F, T, T, T, T, F, F, T, F, T, T, T, T, T, T, T, T, F, T)

fit_ord <- princals(cluster_df_na, ndim = 2, ordinal = ordinals, knots = knotsGifi(cluster_df, "E"), verbose = T)

fit_ord_sum <- summary(fit_ord)

##### Plots ######

pdf("transplot.pdf")
# Takes each factor and shows the transfromed values
plot(fit_ord, plot.type = "transplot")
dev.off()

pdf("loadplot.pdf")
# Plot the loadings of PC1 vs PC2
plot(fit_ord, plot.type = "loadplot")
dev.off()

pdf("biplot.pdf")
# Make bi-plot
plot(fit_ord, plot.type = "biplot", col.scores = "coral", col.loadings = "black")
dev.off()

pdf("screeplot.pdf")
# Scree Plot
plot(fit_ord, plot.type = "screeplot")
dev.off()

##### Observations #####

obs_col <- c(17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 72, 63, 74, 86)

obs_df <- full_data_na[,obs_col]

co_ord <- as.data.frame(fit_ord$objectscores)

cluster_coords <- cbind(obs_df, co_ord)

##### Obs Plots #####
pdf("Combine_Outcome.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = Combine_Outcome)) +
  geom_point()
dev.off()

pdf("lsi_lerner_time.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = lsi_lerner_time)) +
  geom_point()
dev.off()

pdf("ISS_y.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = ISS_y)) +
  geom_point()
dev.off()

pdf("P1_T_or_F.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = P1_T_or_F)) +
  geom_point()
dev.off()

pdf("PATIENT_AGE.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = PATIENT_AGE)) +
  geom_point()
dev.off()

pdf("PATIENT_GENDER.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = PATIENT_GENDER)) +
  geom_point()
dev.off()

pdf("INJURY_MECHANISM.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = INJURY_MECHANISM)) +
  geom_point()
dev.off()

pdf("INJURY_TYPE.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = INJURY_TYPE)) +
  geom_point()
dev.off()

pdf("ASSESS_SYSBP_VAL.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = ASSESS_SYSBP_VAL)) +
  geom_point()
dev.off()

pdf("ASSESS_GCS_TOTAL.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = ASSESS_GCS_TOTAL)) +
  geom_point()
dev.off()

pdf("Has_Lerner_LSI_5_Lerner_time.pdf")
ggplot(cluster_coords, aes(x = D1, y = D2, colour = Has_Lerner_LSI_5_Lerner_time)) +
  geom_point()
dev.off()

##### Saving #####

save(list = ls(), file = "everything")
