##### Setting up environment #####

library(dplyr)
library(Amelia)
library(plyr)
library(ggplot2)
library(forcats)

full_data <- read.csv("Physio_withGOS_AIS_England_Wales_FinalVersion.csv", header = T, check.names = F, na.strings = "", row.names = 1)
full_data <- full_data[,-1]

##### Investigate Missingness #####

missings <- as.data.frame(colSums(is.na(full_data)))
missings$Percentage_missing <- (missings$`colSums(is.na(full_data))`)/nrow(full_data)*100
colnames(missings) <- c("Num_missing", "Percentage_missing")
#missmap(full_data)

ggplot(missings, aes(x = row.names(missings), y = Percentage_missing)) +
  geom_bar(stat = 'identity')

sorted_missings <- missings[order(missings$Percentage_missing),]
sorted_missings$index <- c(1:79)
ggplot(sorted_missings, aes(x = index, y = Percentage_missing)) +
  geom_point() +
  labs(title = "Missingness per Variable in Full Unprocessed Data")

##### Make NA explicit for Factor by Factor Investigation #####

cat_var <- c(3,6:8,13:15,18:19,23,25:28,32:38,40,41:47,49:50,51:53,55:61,72:74,76:77,79)

for (i in 1:length(cat_var)) {
  var <- cat_var[[i]]
  print(var)
  full_data[[var]] <- fct_explicit_na(full_data[[var]])
}

as.data.frame(table(full_data$INJURY_TYPE))

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

num_var <- c(2,4,5,9:12,16,17,20:22,24,29:31,39,48,54,62:71,75,78)

for (i in 1:length(num_var)) {
  var <- num_var[[i]]
  nam <- colnames(full_data[num_var[i]])
  count_na$Column <- addLevel(count_na$Column, nam)
  count_na[i,1] <- colnames(full_data[var])
  count_na[i,2] <- sum(is.na(full_data[var]))
}

