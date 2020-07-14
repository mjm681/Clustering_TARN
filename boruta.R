##### Setting Up Environment #####

library(Boruta)

load("2_cluster_6") # File to be loaded

cluster_data <- data_clusters

##### Feature Selection #####

# Train Model
boruta <- Boruta(cluster~., data = cluster_data, doTrace = 2, maxRuns = 50)
print(boruta)

# Tentative Fix 
boruta_tentative <- TentativeRoughFix(boruta) # Get rid of tentative but weaker
print(boruta_tentative)

boruta_to_use <- boruta_tentative

samp_num <- "s2"
# Plot
pdf_nam <- paste(samp_num, "_variable_importance.pdf", sep = "")
pdf(pdf_nam)
par(mar=c(13,6,4,1)+.1)
cex.axis = 1.5
cex.lab = 2
plot(boruta_to_use, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta_to_use$ImpHistory),function(i)
  boruta_to_use$ImpHistory[is.finite(boruta_to_use$ImpHistory[,i]),i])
names(lz) <- colnames(boruta_to_use$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta_to_use$ImpHistory), cex.axis = 0.7)
title("Sample 2 Feature Importance Determination")
dev.off()

# Create vector of important variables
getSelectedAttributes(boruta, withTentative = F)

# Extract stats of feature importance
imp_stats <- attStats(boruta)
print(imp_stats)

# Sort stats
attach(imp_stats)
imp_stats_sorted <- imp_stats[order(-imp_stats$meanImp),]
detach(imp_stats)

imp_nam <- paste(samp_num, "feature_imp.csv")
write.csv(imp_stats_sorted, imp_nam)

# Exract formula including important variables 
getConfirmedFormula(boruta)

save(list = ls(), file = "boruta")
