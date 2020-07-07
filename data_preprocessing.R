##### Setting Up Environment ##### 

full_data <- read.csv("Physio_withGOS_AIS_England_Wales_FinalVersion.csv", header = T, check.names = F, na.strings = "", row.names = 1) # Make rownames from indices column
full_data <- full_data[,-1] # Remove the extra indices column

##### Writing Function for Adding New Factor Level ##### 

addLevel <- function(x, newlevel=NULL){ 
  if(is.factor(x)) {
    return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

### Airway and Breathing ####

# Col 18 = ASSESS_AIRWAY_VAL
# Col 34 = AIRWAY SUPPORT
# Col 19 = BREATHING STATUS 
# Col 26 = BREATHING SUPPORT


# Change AIRWAY SUPPORT NA to "Not Needed" if ASSESS_AIRWAYS_VAL is "Normal"

full_data$`AIRWAY SUPPORT` <- addLevel(full_data$`AIRWAY SUPPORT`, "Not Needed")
sum((full_data$ASSESS_AIRWAYS_VAL %in% "Normal") & is.na(full_data$`AIRWAY SUPPORT`)) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( (full_data[i,18] %in% "Normal") & (is.na(full_data[i,34]) == TRUE ) ) {
    full_data[i,34] <- "Not Needed"
  }
}


# Change ASSESS_AIRWAYS_VAL to "Not Recorded" if AIRWAY SUPPORT has a value but ASSESS_AIRWAYS_VAL = NA

full_data$ASSESS_AIRWAYS_VAL <- addLevel(full_data$ASSESS_AIRWAYS_VAL, "Not Recorded")
sum((is.na(full_data$ASSESS_AIRWAYS_VAL)) & !is.na(full_data$`AIRWAY SUPPORT`)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,18]) == TRUE) & ( is.na(full_data[i,34]) == FALSE ) ) {
    full_data[i,18] <- "Not Recorded"
  }
}


# Change BREATHING STATUS NA to "Not Needed" if BREATHING SUPPORT  is "Normal"

full_data$`BREATHING SUPPORT` <- addLevel(full_data$`BREATHING SUPPORT`, "Not Needed")
sum((full_data$`BREATHING STATUS` %in% "Normal") & is.na(full_data$`BREATHING SUPPORT`)) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( ((full_data[i,19] %in% "Normal")) & (is.na(full_data[i,26]) == TRUE ) ) {
    full_data[i,26] <- "Not Needed"
  }
}


# Change BREATHING STATUS from NA to "Not Recorded" if BREATHING SUPPORT is present but BREATHING STATUS = NA

full_data$`BREATHING STATUS` <- addLevel(full_data$`BREATHING STATUS`, "Not Recorded")
sum(is.na(full_data$`BREATHING STATUS`) & !is.na(full_data$`BREATHING SUPPORT`)) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,19]) == TRUE) & ( is.na(full_data[i,26]) == FALSE ) ) {
    full_data[i,19] <- "Not Recorded"
  }
}

full_data_chk1 <- full_data

# Create new boolean variable "Received airway support"
full_data$`Received Airway/Breathing Support` <- NA

# In the case of certain airway/breathing conditions, new Received Airway/Breathing Support variable is TRUE, else is FALSE
for (i in 1:nrow(full_data)) {
  if ( (full_data[i,18] %in% "Supported") | (full_data[i,18] %in% "Obstructed") | (full_data[i,18] %in% "Intubated") | 
       (full_data[i,18] %in% "Cricothyroidotomy") | (full_data[i,18] %in% "Tracheostomy") | (full_data[i,34] %in% "Intubation") |
       (full_data[i,34] %in% "Cricothyroidotomy") | (full_data[i,34] %in% "Tracheostomy") | (full_data[i,26] %in% "Mechanical Ventilation") |
       (full_data[i,26] %in% "cpap") | (full_data[i,26] %in% "bipap") ) {
    full_data[i,80] <- TRUE
  }
  else {
    full_data[i,80] <- FALSE
  }
}

as.data.frame(table(full_data$`Received Airway/Breathing Support`)) # Checking how many cases are affected 

### Analgesia ####

# Col 13 = ANALGESIA_ANALG
# Col 14 = ANALGESIA_CONTIN
# Col 15 = ANALGESIA_DRUG


# Change ANALGESIA_CONTIN NA to "Not Recorded" if ANALGESIA_ANALG has a value but ANALGESIA_CONTIN = NA

#full_data$ANALGESIA_CONTIN <- addLevel(full_data$ANALGESIA_CONTIN, "Not Recorded")
sum((is.na(full_data$ANALGESIA_CONTIN)) & (!is.na(full_data$ANALGESIA_ANALG))) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,13]) == FALSE) & ( is.na(full_data[i,14]) == TRUE ) ) {
    full_data[i,14] <- "Not Recorded"
  }
}


# Change ANALGESIA_CONTIN to NA if ANALGESIA_CONTIN is "No" but ANALGESIA_ANALG = NA and ANALGESIA_DRUG = NA

sum((is.na(full_data$ANALGESIA_DRUG)) & (is.na(full_data$ANALGESIA_ANALG)) & ( full_data$ANALGESIA_CONTIN %in% "No" )) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,13]) == TRUE) & ( full_data[i,14] %in% "No" ) & ( is.na(full_data[i,15]) == TRUE )) {
    full_data[i,14] <- NA
  }
}


# Change ANALGESIA_ANALG and ANALGESIA_DRUG to "Not Recorded" if ANALGESIA_CONTIN is "Yes" but ANALGESIA_ANALG = NA and ANALGESIA_DRUG = NA
sum((is.na(full_data$ANALGESIA_DRUG)) & (is.na(full_data$ANALGESIA_ANALG)) & ( full_data$ANALGESIA_CONTIN %in% "Yes" )) # Checking how many cases are affected 

full_data$ANALGESIA_DRUG <- addLevel(full_data$ANALGESIA_DRUG, "Not Recorded")
full_data$ANALGESIA_ANALG <- addLevel(full_data$ANALGESIA_ANALG, "Not Recorded")

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,13]) == TRUE) & ( full_data[i,14] %in% "Yes" ) & ( is.na(full_data[i,15]) == TRUE )) {
    full_data[i,13] <- "Not Recorded"
    full_data[i,15] <- "Not Recorded"
  }
}


# Change all Analgesia factors to "Not Recorded" if all 3 are NA

sum(is.na(full_data$ANALGESIA_ANALG) & is.na(full_data$ANALGESIA_DRUG) & is.na(full_data$ANALGESIA_CONTIN)) # Checking how many cases are affected 

full_data$ANALGESIA_DRUG <- addLevel(full_data$ANALGESIA_DRUG, "Not Received")
full_data$ANALGESIA_CONTIN <- addLevel(full_data$ANALGESIA_CONTIN, "Not Received")
full_data$ANALGESIA_ANALG <- addLevel(full_data$ANALGESIA_ANALG, "Not Received")

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,13]) == TRUE) & (is.na(full_data[i,14]) == TRUE) & (is.na(full_data[i,15]) == TRUE) ) {
    full_data[i,13] <- "Not Received"
    full_data[i,14] <- "Not Received"
    full_data[i,15] <- "Not Received"
  }
}

### Blood Pressure ####

# Col 16 - ASSESS_DIABP_VAL
# Col 17 = ASSESS_SYSBP_VAL

# Filter cases where ASSESS_SYSBP_VAL < ASSESS_DIABP_VAL

for (i in 1:nrow(full_data)) {
  if ( (isTRUE(full_data[i,17] < full_data[i,16])) ) {
    full_data <- full_data[-i,]
  }
}

nrow(full_data) # Checking how many cases are affected 

### Blood Products ####

# Col 53 = BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_TNSTYPE
# Col 54 = BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_UNITS


# If units is present but type is missing, convert type to "Not Recorded"

full_data$`BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_TNSTYPE` <- addLevel(full_data$`BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_TNSTYPE`, "Not Recorded")

sum((is.na(full_data$`BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_TNSTYPE`)) & (!is.na(full_data$`BLOOD PRODUCTS IN FIRST 24 HOURS_FLUID_UNITS`))) # Checking how many cases are affected 

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,53]) == TRUE) & ( is.na(full_data[i,54]) == FALSE ) ) {
    full_data[i,53] <- "Not Recorded"
  }
}

### Capillary Refill Time ####

# Col 44 = ASSESS_CREFILL_NORM
# Col 73 = CAPTIME_REVISED_IsMoreThan2


# Change CAPTIME_REVISED_IsMoreThan2 to "Not Recorded" if ASSESS_CREFILL_NORM is present but CAPTIME_REVISED_IsMoreThan2 = NA

full_data$CAPTIME_REVISED_IsMoreThan2 <- addLevel(full_data$CAPTIME_REVISED_IsMoreThan2, "Not Recorded")
sum(!is.na(full_data$ASSESS_CREFILL_NORM) & is.na(full_data$CAPTIME_REVISED_IsMoreThan2)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,44]) == FALSE) & ( is.na(full_data[i,73]) == TRUE ) ) {
    full_data[i,73] <- "Not Recorded"
  }
}


# Change ASSESS_CREFILL_NORM to "Not Recorded" if CAPTIME_REVISED_IsMoreThan2 is present but ASSESS_CREFILL_NORM = NA

full_data$ASSESS_CREFILL_NORM <- addLevel(full_data$ASSESS_CREFILL_NORM, "Not Recorded")
sum(is.na(full_data$ASSESS_CREFILL_NORM) & !is.na(full_data$CAPTIME_REVISED_IsMoreThan2)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,44]) == TRUE) & ( is.na(full_data[i,73]) == FALSE ) ) {
    full_data[i,44] <- "Not Recorded"
  }
}

# If either ASSESS_CREFILL_NORM or CAPTIME_REVISED_IsMoreThan2 is missing, convert NA --> "Not Recorded

sum(is.na(full_data$ASSESS_CREFILL_NORM)) # Checking how many cases are affected
sum(is.na(full_data$CAPTIME_REVISED_IsMoreThan2)) # Checking how many cases are affected

full_data$ASSESS_CREFILL_NORM <- addLevel(full_data$ASSESS_CREFILL_NORM, "Not Recorded")
full_data$CAPTIME_REVISED_IsMoreThan2 <- addLevel(full_data$CAPTIME_REVISED_IsMoreThan2, "Not Recorded")

for (i in 1:nrow(full_data)) {
  if (isTRUE(is.na(full_data[i,44]))) {
    full_data[i,44] <- "Not Recorded"
  }
  if (isTRUE(is.na(full_data[i,73]))) {
    full_data[i,73] <- "Not Recorded"
  }
}


# Add new column "Conflicting Cap Refill Values" for conflicting ASSESS_CREFILL_NORM and CAPTIME_REVISED_IsMoreThan2 values

full_data$`Conflicting Cap Refill Values` <- NA

for (i in 1:nrow(full_data)) {
  if ( ((full_data[i,44] %in% "Normal") & (full_data[i,73] %in% "True" )) | ((full_data[i,44] %in% "Abnormal") & (full_data[i,73] %in% "False" )) ) {
    full_data[i,81] <- TRUE
  }
  else ( full_data[i,81] <- FALSE )
}

as.data.frame(table(full_data$`Conflicting Cap Refill Values`)) # Checking how many cases are affected

### Fluid Type ####

# Col 23 = FLUID_BLSTYPE
# Col 24 = FLUID_VOL


# Change FLUID_BLSTYPE to "Not Recorded" if FLUID_VOL is present and FLUID_BLSTYPE = NA

sum(is.na(full_data$FLUID_BLSTYPE) & !is.na(full_data$FLUID_VOL)) # Checking how many cases are affected

full_data$FLUID_BLSTYPE <- addLevel(full_data$FLUID_BLSTYPE, "Not Recorded")

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,23]) == TRUE) & ( is.na(full_data[i,24]) == FALSE ) ) {
    full_data[i,23] <- "Not Recorded"
  }
}


# Change FLUID_BLSTYPE and FLUID_VOL to NA if FLUID_VOL = 0

as.data.frame(table(full_data$FLUID_VOL)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( full_data[i,24] %in% 0 ) {
    full_data[i,24] <- NA
    full_data[i,23] <- NA
  }
}


# Change FLUID_BLSTYPE to "Not Received" if FLUID_BLSTYPE and FLUID_VOL = NA

full_data$FLUID_BLSTYPE <- addLevel(full_data$FLUID_BLSTYPE, "Not Received")

for (i in 1:nrow(full_data)) {
  if ( (is.na(full_data[i,23]) == TRUE) & ( is.na(full_data[i,24]) == TRUE ) ) {
    full_data[i,23] <- "Not Received"
  }
}

as.data.frame(table(full_data$FLUID_BLSTYPE)) # Checking how many cases are affected

### GCS ####

full_data_chk2 <- full_data

# Col 12 = ASSESS_GCS_TOTAL
# Col 20 = ASSESS_GCS_EYE
# Col 21 = ASSESS_GCS_MOTOR
# Col 22 = ASSESS_GCS_VERBAL


# Filter cases with GCS Scores not on the Scale

as.data.frame(table(full_data$ASSESS_GCS_EYE)) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_MOTOR)) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_VERBAL)) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_TOTAL)) # Checking how many cases are affected

full_data <- full_data[which(full_data$ASSESS_GCS_EYE<=4 | is.na(full_data$ASSESS_GCS_EYE)),]
full_data <- full_data[which(full_data$ASSESS_GCS_MOTOR<=6 | is.na(full_data$ASSESS_GCS_MOTOR)),]
full_data <- full_data[which(full_data$ASSESS_GCS_VERBAL<=5 | is.na(full_data$ASSESS_GCS_VERBAL)),]
full_data <- full_data[which(full_data$ASSESS_GCS_TOTAL<=15 | is.na(full_data$ASSESS_GCS_TOTAL)),]
full_data <- full_data[which(full_data$ASSESS_GCS_TOTAL>0 | is.na(full_data$ASSESS_GCS_TOTAL)),]


# If ASSESS_GCS_TOTAL = 0 and any of the constituents are NA, replace NA with 0

sum((full_data$ASSESS_GCS_TOTAL %in% 3)) # Checking how many cases are affected
sum(is.na(full_data$ASSESS_GCS_EYE)) # Checking how many cases are affected
sum(is.na(full_data$ASSESS_GCS_MOTOR)) # Checking how many cases are affected
sum(is.na(full_data$ASSESS_GCS_VERBAL)) # Checking how many cases are affected

for ( i in 1:nrow(full_data) ) {
  if ( full_data[i,12] %in% 3 & is.na(full_data[i,20]) == TRUE ) {
    full_data[i,20] <- 1
  }
  if ( full_data[i,12] %in% 3 & is.na(full_data[i,21]) == TRUE ) {
    full_data[i,21] <- 1
  }
  if ( full_data[i,12] %in% 3 & is.na(full_data[i,22]) == TRUE ) {
    full_data[i,22] <- 1
  }
}


# If ASSESS_GCS_TOTAL = NA, calculate ASSESS_GCS_TOTAL as ASSESS_GCS_MOTOR + ASSESS_GCS_VERBAL + ASSESS_GCS_EYE (so long as none are missing)

sum(is.na(full_data$ASSESS_GCS_TOTAL)) # Checking how many cases are affected

for ( i in 1:nrow(full_data) ) {
  if ( is.na(full_data[i,21]) | is.na(full_data[i,22]) | is.na(full_data[i,20]) ) {
    next
  }
  if ( isTRUE(is.na(full_data[i,12])) ) {
    full_data[i,12] <- (sum(full_data[i,21], na.rm = T) + sum(full_data[i,22], na.rm = T) + sum(full_data[i,20], na.rm = T))
  }
}


# If ASSESS_GCS_TOTAL = 15 and any of the components = NA, change it to max on the scale

sum(full_data$ASSESS_GCS_TOTAL %in% 15) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_EYE)) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_MOTOR)) # Checking how many cases are affected
as.data.frame(table(full_data$ASSESS_GCS_VERBAL)) # Checking how many cases are affected

for ( i in 1:nrow(full_data) ) {
  if ( full_data[i,12] %in% 15 & is.na(full_data[i,20]) == TRUE ) {
    full_data[i,20] <- 4
  }
  if ( full_data[i,12] %in% 15 & is.na(full_data[i,21]) == TRUE ) {
    full_data[i,21] <- 6
  }
  if ( full_data[i,12] %in% 15 & is.na(full_data[i,22]) == TRUE ) {
    full_data[i,22] <- 5
  }
}


full_data_chk3 <- full_data

# For all the cases where there is no missing constituents, filter out any where ASSESS_GCS_TOTAL does not = sum of constituents

for ( i in 1:nrow(full_data) ) {
  if ( is.na(full_data[i,21]) | is.na(full_data[i,22]) | is.na(full_data[i,20]) ) {
    next
  }
  if (!isTRUE((sum(full_data[i,20], na.rm = T) + sum(full_data[i,21], na.rm = T) + sum(full_data[i,22], na.rm = T)) == (full_data[i,12]))) {
    full_data <- full_data[-i,]
  }
}

full_data_chk4 <- full_data

### Operative Procedure ####

# Col 56 = OPERATIVE PROCEDURE_PROC
# Col 59 = OPERATIVE PROCEDURE_SURG_SPEC


# Remove the numbers from OPERATIVE PROCEDURE_PROC, assume it is error

as.data.frame(table(full_data$`OPERATIVE PROCEDURE_PROC`)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( full_data[i,56] %in% "567" ) {
    full_data[i,56] <- NA
  }
  if ( full_data[i,56] %in% "816" ) {
    full_data[i,56] <- NA
  }
}

full_data$`OPERATIVE PROCEDURE_PROC` <- droplevels(full_data$`OPERATIVE PROCEDURE_PROC`)


# Remove the "Chest" procedures from OPERATIVE PROCEDURE_SURG_SPEC, assume it is error

as.data.frame(table(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`)) # Checking how many cases are affected

for (i in 1:nrow(full_data)) {
  if ( full_data[i,59] %in% "Chest" ) {
    full_data[i,59] <- NA
  }
}

full_data$`OPERATIVE PROCEDURE_SURG_SPEC` <- droplevels(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`)

# Standardise the numeric values in OPERATIVE PROCEDURE_SURG_SPEC

as.data.frame(table(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`)) # Checking how many cases are affected

full_data$`OPERATIVE PROCEDURE_SURG_SPEC` <- addLevel(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`, 2)

for ( i in 1:nrow(full_data)) {
  if ( full_data[i,59] %in% "1.0" ) {
    full_data[i,59] <- 1
  }
  if ( full_data[i,59] %in% "2.0" ) {
    full_data[i,59] <- 2
  }
  if ( full_data[i,59] %in% "16.0" ) {
    full_data[i,59] <- 16
  }
  if ( full_data[i,59] %in% "21.0" ) {
    full_data[i,59] <- 21
  }
  if ( full_data[i,59] %in% "99.0" ) {
    full_data[i,59] <- 99
  }
}

full_data$`OPERATIVE PROCEDURE_SURG_SPEC` <- droplevels(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`)

full_data$`OPERATIVE PROCEDURE_SURG_SPEC` <- as.numeric(levels(full_data$`OPERATIVE PROCEDURE_SURG_SPEC`))[full_data$`OPERATIVE PROCEDURE_SURG_SPEC`]

full_data_chk5 <- full_data

### Dates to Received Treatment/Investigation #####

# Date of treatment/investigation is included, convert to "Received". If not received, NA = "Not Received"

# Columns: LIMB SPLINT, CARDIO RESPIRATORY RESUSCITATION, ECG (12 LEAD), CHEST DRAIN, DIRECT COMPRESSION OF EXTERNAL HAEMORRHAGE, SEDATION/ANAESTHESIA, ANTICONVULSAN_DATE, INOTROPES/VASOSUPRESSORS, THORACOSTOMY, WARMING
dates <- c(32, 35, 46, 47, 49, 50, 51, 52, 55, 61)

for (date in 1:10) {
  d <- dates[date]
  full_data[[d]] <- addLevel(full_data[[d]], "Received")
  full_data[[d]] <- addLevel(full_data[[d]], "Not Received")
}

# TAKES AGES
for (i in 1:nrow(full_data)) {
  for (date in 1:10) {
    d <- dates[date]
    if ( (is.na(full_data[i,d]) == FALSE) ) {
      full_data[i,d] <- "Received" 
    } 
    if ( (is.na(full_data[i,d]) == TRUE) ) {
      full_data[i,d] <- "Not Received" 
    } 
  }
}

full_data_chk6 <- full_data

full_data$`LIMB SPLINT` <- droplevels(full_data$`LIMB SPLINT`)
as.data.frame(table(full_data$`LIMB SPLINT`)) # Checking how many cases are affected

full_data$`CARDIO RESPIRATORY RESUSCITATION` <- droplevels(full_data$`CARDIO RESPIRATORY RESUSCITATION`)
as.data.frame(table(full_data$`CARDIO RESPIRATORY RESUSCITATION`)) # Checking how many cases are affected

full_data$`ECG (12 LEAD)` <- droplevels(full_data$`ECG (12 LEAD)`)
as.data.frame(table(full_data$`ECG (12 LEAD)`)) # Checking how many cases are affected

full_data$`CHEST DRAIN` <- droplevels(full_data$`CHEST DRAIN`)
as.data.frame(table(full_data$`CHEST DRAIN`)) # Checking how many cases are affected

full_data$`DIRECT COMPRESSION OF EXTERNAL HAEMORRHAGE` <- droplevels(full_data$`DIRECT COMPRESSION OF EXTERNAL HAEMORRHAGE`)
as.data.frame(table(full_data$`DIRECT COMPRESSION OF EXTERNAL HAEMORRHAGE`)) # Checking how many cases are affected

full_data$`SEDATION/ANAESTHESIA` <- droplevels(full_data$`SEDATION/ANAESTHESIA`)
as.data.frame(table(full_data$`SEDATION/ANAESTHESIA`)) # Checking how many cases are affected

full_data$ANTICONVULSAN_DATE <- droplevels(full_data$ANTICONVULSAN_DATE)
as.data.frame(table(full_data$ANTICONVULSAN_DATE)) # Checking how many cases are affected

full_data$`INOTROPES/VASOSUPRESSORS` <- droplevels(full_data$`INOTROPES/VASOSUPRESSORS`)
as.data.frame(table(full_data$`INOTROPES/VASOSUPRESSORS`)) # Checking how many cases are affected

full_data$THORACOSTOMY <- droplevels(full_data$THORACOSTOMY)
as.data.frame(table(full_data$THORACOSTOMY)) # Checking how many cases are affected

full_data$WARMING <- droplevels(full_data$WARMING)
as.data.frame(table(full_data$WARMING)) # Checking how many cases are affected

full_data_chk7 <- full_data

### Critical Care #####

# Make new columns concerning if they arrived at critical care and if they left

full_data$CC_Arrived <- NA
full_data$CC_Departed <- NA

for (i in 1:nrow(full_data)) {
  if ( is.na(full_data[i,27]) == FALSE ) {
    full_data[i,82] <- TRUE
  }
  else {
    full_data[i,82] <- FALSE
  }
  if ( is.na(full_data[i,42]) == FALSE ) {
    full_data[i,83] <- TRUE
  }
  if ( (is.na(full_data[i,27]) == FALSE) & (is.na(full_data[i,42]) == TRUE) ) {
    full_data[i,83] <- FALSE
  }
}

as.data.frame(table(full_data$CC_Arrived)) # Checking how many cases are affected
as.data.frame(table(full_data$CC_Departed)) # Checking how many cases are affected


# Combining Critical Care data into CC_Summary to investigate the conflict between CC_STAY and CC_Arrived

full_data$CC_Summary <- paste(full_data$CC_Arrived, full_data$CC_Departed, full_data$CCARE_STAY)

as.data.frame(table(full_data$CC_Summary)) # Checking how many cases are affected


full_data_chk8 <- full_data

##### LSI #####

# Col 76 = Has_Lerner_LSI_5_Lerner_time
# Col 79 = Has_Lerner_LSI_5_Lerner_time_peads

# Convert factors "True" and "False" to Boolean variables

as.data.frame(table(full_data$Has_Lerner_LSI_5_Lerner_time)) # Checking how many cases are affected
as.data.frame(table(full_data$Has_Lerner_LSI_5_Lerner_time_peads)) # Checking how many cases are affected

full_data$Has_Lerner_LSI_5_Lerner_time <- addLevel(full_data$Has_Lerner_LSI_5_Lerner_time, TRUE)
full_data$Has_Lerner_LSI_5_Lerner_time <- addLevel(full_data$Has_Lerner_LSI_5_Lerner_time, FALSE)
full_data$Has_Lerner_LSI_5_Lerner_time_peads <- addLevel(full_data$Has_Lerner_LSI_5_Lerner_time_peads, TRUE)
full_data$Has_Lerner_LSI_5_Lerner_time_peads <- addLevel(full_data$Has_Lerner_LSI_5_Lerner_time_peads, FALSE)

for ( i in 1:nrow(full_data)) {
  if ( full_data[i,76] %in% "False" ) {
    full_data[i,76] <- FALSE
  }
  if ( full_data[i,79] %in% "False" ) {
    full_data[i,79] <- FALSE
  }
  if ( full_data[i,76] %in% "True" ) {
    full_data[i,76] <- TRUE
  }
  if ( full_data[i,79] %in% "True" ) {
    full_data[i,79] <- TRUE
  }
}

full_data$Has_Lerner_LSI_5_Lerner_time <- droplevels(full_data$Has_Lerner_LSI_5_Lerner_time)
full_data$Has_Lerner_LSI_5_Lerner_time_peads <- droplevels(full_data$Has_Lerner_LSI_5_Lerner_time_peads)

full_data$Has_Lerner_LSI_5_Lerner_time <- as.logical(full_data$Has_Lerner_LSI_5_Lerner_time)
full_data$Has_Lerner_LSI_5_Lerner_time_peads <- as.logical(full_data$Has_Lerner_LSI_5_Lerner_time_peads)

### ISS and AIS ####

# Add new factor for ISS_15andOver

full_data$ISS_15andOver <- NA

for (i in 1:nrow(full_data)) {
  if ( isTRUE(full_data[i,63] >= 15) ) {
    full_data[i,85] <- TRUE
  }
  if ( (isTRUE(full_data[i,63] < 15)) & ( !is.na(full_data[i,63])) ) {
    full_data[i,85] <- FALSE
  }
}

as.data.frame(table(full_data$ISS_15andOver))

full_data_chk9 <- full_data

##### 

# Add new factor to summarise if it is P1 or not P1

full_data$P1_T_or_F <- NA

for (i in 1:nrow(full_data)) {
  if ( full_data[i, 74] %in% "P1" == T ) {
    full_data[i, 86] <- TRUE
  }
  else ( full_data[i, 86] <- FALSE )
}

full_data_chk10 <- full_data

##### ISS #####

# Filter out cases where ISS is not calculated correctly

not_right <- full_data[0,]
right <- full_data[0,]

for (i in 1:nrow(full_data)) {
  x <- c(full_data[i,64],full_data[i,65],full_data[i,66],
         full_data[i,67],full_data[i,68],full_data[i,69],
         full_data[i,70],full_data[i,71])
  n <- as.numeric(length(x))
  A <- sort(x, partial = n)[n] # Finding largest score
  B <- sort(x, partial = n-1)[n-1] # Finding second largest score
  C <- sort(x, partial = n-2)[n-2] # Finding third largest score
  iss_calc <- sum((A^2), (B^2), (C^2)) # Combine square of top 3 scores for ISS total
  if ( 6 %in% x == TRUE ) { # If any score is 6 (max), ISS is assigneed 75 
    iss_calc <- 75
   }
  if ( !isTRUE(iss_calc == full_data[i,63] )) {
    not_right <- as.data.frame(rbind(not_right, full_data[i,])) # Store incorrect cases
  }
  else {
    right <- as.data.frame(rbind(right, full_data[i,])) # Store correct cases 
  }
}

index_right <- row.names(right)
index_not_right <- row.names(not_right)

intersection <- intersect(index_right, index_not_right) # Checking there is no overlap

full_data <- right

full_data_chk11 <- full_data

##### Filtering to Inclusive #####

full_data <- subset(full_data, PATIENT_AGE >= 16)

full_data <- full_data[!is.na(full_data$ASSESS_RESP_RATE_VAL),]
full_data <- full_data[!is.na(full_data$ASSESS_PULSE_VAL),]
full_data <- full_data[!is.na(full_data$ASSESS_GCS_TOTAL),]
full_data <- full_data[!is.na(full_data$ASSESS_SYSBP_VAL),]
full_data <- full_data[!is.na(full_data$ASSESS_GCS_MOTOR),]

full_data_chk12 <- full_data

##### Saving Processed Dataset #####

write.csv(full_data, "final_data.csv")

##### Saving Result #####

save(list = ls(), file = "everything")
