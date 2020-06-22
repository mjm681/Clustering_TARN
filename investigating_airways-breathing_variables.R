##### Load Data and Set Up Environment #####

library(forcats)
full_data <- read.csv("Physio_withGOS_AIS_England_Wales_FinalVersion.csv", header = T, check.names = F, na.strings = "", row.names = 1)
full_data <- full_data[,-1]

##### Investigate Airway and Breathing Variables #####

### Airway

airway_status <- data.frame(full_data$ASSESS_AIRWAYS_VAL)
airway_support <- data.frame(full_data$`AIRWAY SUPPORT`)

airway_df <- cbind(airway_status, airway_support)
colnames(airway_df) <- c("Airway_Status", "Airway_Support")

airway_df$Merged <- paste(airway_df$Airway_Status, airway_df$Airway_Support)

airway_df$Merged <- fct_explicit_na(airway_df$Merged)
factor_merged_airway <- as.data.frame(table(airway_df$Merged))


factor_airway_status <- as.data.frame(table(airway_status))
factor_airway_support <- as.data.frame(table(airway_support))
#write.csv(factor_merged_airway, file = "factor_merged_airway.csv")

### Breathing

breathing_status <- data.frame(full_data$`BREATHING STATUS`)
breathing_support <- data.frame(full_data$`BREATHING SUPPORT`)

breathing_df <- cbind(breathing_status, breathing_support)
colnames(breathing_df) <- c("Breathing_Status", "Breathing_Support")

breathing_df$Merged <- paste(breathing_df$Breathing_Status, breathing_df$Breathing_Support)

breathing_df$Merged <- fct_explicit_na(breathing_df$Merged)
factor_merged_breathing <- as.data.frame(table(breathing_df$Merged))

#write.csv(factor_merged_breathing, file = "factor_merged_breathing.csv")


##### Adding same levels #####

addLevel <- function(x, newlevel=NULL){ 
  if(is.factor(x)) {
    return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

breathing_df$Breathing_Status <- addLevel(breathing_df$Breathing_Status, "Mechanical Ventilation")
breathing_df$Breathing_Status <- addLevel(breathing_df$Breathing_Status, "BiPAP")
breathing_df$Breathing_Status <- addLevel(breathing_df$Breathing_Status, "Bronchodilator")

breathing_df$Breathing_Support <- addLevel(breathing_df$Breathing_Support, "Mechanical ventilation")
breathing_df$Breathing_Support <- addLevel(breathing_df$Breathing_Support, "Non-invasive ventilation")
breathing_df$Breathing_Support <- addLevel(breathing_df$Breathing_Support, "Normal")
breathing_df$Breathing_Support <- addLevel(breathing_df$Breathing_Support, "Respiratory arrest")
breathing_df$Breathing_Support <- addLevel(breathing_df$Breathing_Support, "Respiratory distress")

##### Replacing "Mechanical Ventilation" with "Mechanical ventilation" #####

for (i in 1:nrow(airway_df)) {
  if ( (airway_df[i,2] %in% "Mechanical Ventilation") ) {
    airway_df[i,2] <- "Mechanical ventilation"
  }
}

##### Replacing "Mechanical Ventilation" with "Mechanical ventilation" #####

for (i in 1:nrow(breathing_df)) {
  if ( (breathing_df[i,2] %in% "Mechanical Ventilation") ) {
    breathing_df[i,2] <- "Mechanical ventilation"
  }
}

##### Assessing Overlap ####

subset_same <- subset(breathing_df, Breathing_Status == Breathing_Support)

subset_normal <- subset(breathing_df, Breathing_Status == "Normal")
subset_NA_NA <- subset(breathing_df, is.na(Breathing_Status) == T & is.na(Breathing_Support) == T )
subset_normal_NA <- subset(breathing_df, Breathing_Status == "Normal" & is.na(Breathing_Support) == T)

subset_same$Merged <- paste(subset_same$Breathing_Status, subset_same$Breathing_Support)
subset_same$Merged <- fct_explicit_na(subset_same$Merged)
factor_same_breathing_merged <- as.data.frame(table(subset_same$Merged))

subset_any_NA <- subset(breathing_df, is.na(Breathing_Status) == F & Breathing_Status != "Normal" & is.na(Breathing_Support) == T )
subset_any_NA$Merged <- paste(subset_any_NA$Breathing_Status, subset_any_NA$Breathing_Support)
subset_any_NA$Merged <- fct_explicit_na(subset_any_NA$Merged)
factor_any_NA_breathing <- as.data.frame(table(subset_any_NA$Merged))

