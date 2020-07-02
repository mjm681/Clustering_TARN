##### Multiple Clustering Analysis ##### 

MFA_data <- read.csv("final_data.csv", header = T, check.names = F, na.strings = "NA", row.names = 1)

column_names <- c('caseid', 'ASSESS_SYSBP_VAL', 'ASSESS_CREFILL_NORM', 'ASSESS_GCS_MOTOR', 
                  'ASSESS_GCS_TOTAL', 'ASSESS_PULSE_VAL', 'ASSESS_RESP_RATE_VAL', 
                  'INJURY_MECHANISM', 'INJURY_TYPE', 'PATIENT_AGE', 'PATIENT_GENDER', 
                  'CAPTIME_REVISED_IsMoreThan2', 'Head', 'Face', 'Chest', 'Abdomen', 
                  'Pelvis', 'Spine', 'Limb', 'Other', 'Has_Lerner_LSI_5_Lerner_time',
                  'P1_T_or_F')

cols <- c(1, 17, 44, 73, 21, 12, 11, 10, 8, 7, 4, 3, 64:71, 76, 86)

cluster_df <- MFA_data[,cols]
rownames(cluster_df) <- cluster_df$caseid
cluster_df <- cluster_df[,-1]

cluster_df_na <- cluster_df[complete.cases(cluster_df),]

#####

library(FactoMineR)
library(factoextra)

group <- c(1, 2, 2, 1, 1, 2, 1, 1, 8, 1, 1)
name.group <- c("Systolic_BP", "Capillary_Refill", "GCS", "Pulse", "Resp", "Injury", "Age", "Gender", "AIS", "LSI5", "P1_TorF")

type <- c("s", "n", "s", "s", "s", "n", "s", "n", "s", "n", "n")

cluster_df_na$ASSESS_GCS_TOTAL <- as.numeric(cluster_df_na$ASSESS_GCS_TOTAL)

res.mfa <- MFA(cluster_df_na,
               group = c(1, 2, 2, 1, 1, 2, 1, 1, 8, 1, 1), 
               type = c("s", "n", "s", "s", "s", "n", "s", "n", "s", "n", "n"), 
               ind.sup = NULL,
               name.group = c("Systolic_BP", "Capillary_Refill", "GCS", "Pulse", "Resp", "Injury", "Age", "Gender", "AIS", "LSI5", "P1_TorF"), 
               num.group.sup = NULL, 
               graph = TRUE)

print(res.mfa)

# Extract the eigenvalues/variances retained by each dimension (axis).
eig.val <- get_eigenvalue(res.mfa)
head(eig.val)

# Scree Plot
scree <- fviz_screeplot(res.mfa)

# Visualize the eigenvalues/variances
eigh <- fviz_eig(res.mfa)

#Extract the results for individuals.
group <- get_mfa_ind(res.mfa)
group

# Coordinates of groups
head(group$coord)

# Cos2: quality of representation on the factore map
head(group$cos2)

# Contributions to the  dimensions
head(group$contrib)
mfa_var <- fviz_mfa_var(res.mfa, "group")

#Extract the results for quantitative and qualitative variables, as well as, for groups of variables.
get_mfa_var(res.mfa)

#Visualize the results for individuals and variables, respectively.
fviz_ind <- fviz_mfa_ind(res.mfa)
fviz_var <- fviz_mfa_var(res.mfa)

##### Saving #####

save(list = ls(), file = "everything")
