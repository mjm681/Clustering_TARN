# Clustering_TARN: 
# Using Partitional Clustering on the Trauma Audit & Research Network Dataset and Boruta Feature Importance Determination 

The Trauma Audit & Research Network (TARN) dataset is the UK’s National Clinical Audit of trauma and was designed to improve the organisation and treatment of trauma. The TARN dataset is commonly used for research into trauma. It is hypothesised that there are subgroups within the dataset that can be explained by patient characteristics. The TARN dataset requires extensive pre-processing and filtering. Partitional clustering algorithms (partitioning around the medoid, PAM, and Clustering Large Applications, CLARA) are used to investigate the structure of the TARN dataset. Boruta feature importance determination is used to investigate which variables are most important in the clustering result.

*All code written here is my own work. This part of the project was separate from the work of the rest of the Smart Triage Study Group. They supported me in advisory roles only.*

## Data Exploration and Pre-Processing

### TARN_variables.csv

Spreadsheet explaining the variables in the TARN dataset and additional variables that are added during pre-processing stage.

### data_preprocessing.R

Pre-processing and filtering applied to the TARN data in a variable-wise way. 
* **Input:** Raw TARN data (79 variables)  
* **Output:** Pre-pocessed and filtered data (86 variables)

### investigating_variables.R 

Script to assess and plot the missingness of the data and save the summary/investigation into the variables.
* **Input:** Raw TARN data (79 variables), Pre-pocessed and filtered data (86 variables), Clustering data (21 variables)
* **Output:** *data_exploration_raw_data*, *data_exploration_final_data*, *data_exploration_clustering_data*

### investigating_airways-breathing_variables.R

Investigation into ASSESS_BREATHING_STATUS, ASSESS_BREATHING_SUPPORT, ASSESS_AIRWAY_STATUS and ASSESS_AIRWAY_SUPPORT due to inconsistencies.
* **Input:** Pre-pocessed and filtered data (86 variables)
* **Output:** Investigation into breathing/airway variables

### data_exploration_raw_data

Output of the “investigating_variables.R” script for the raw data.

### data_exploration_final_data

Output of the “investigating_variables.R” script for the pre-processed and filtered data.

### data_exploration_clustering_data

Output of the “investigating_variables.R” script for the data used in clustering.

## Data Analysis

### selecting_clustering_variables.R

Short script to select the 21 variables for clustering. 
* **Input:** Pre-pocessed and filtered data (86 variables)  
* **Output:** Clustering data (21 variables)

### NbClust.R

Script to estimate the optimal number of clusters in the data using multiple indices and plot the output.
* **Input:** Clustering data (21 variables)
* **Output:** Table of optimum number of clusters per index and distance combination, bar chart to display the optimum number of clusters

### pam_sampling.R

Iteratively performs PAM clustering using samples of the data and saves the output.
* **Input:** Clustering data (21 variables)
* **Output:** PAM object and whole environment saved per sampling iteration

### clara.R

One-Hot Encodes the data and performs CLARA clustering on One-Hot Encoded data.
* **Input:** Clustering data (21 variables)
* **Output:** CLARA object and whole environment saved

### Boruta.R

Feature importance determination using Boruta (random forest wrapper algorithm).
* **Input:** PAM object or CLARA object
* **Output:** Boruta object of importance of features

## Visualisation 

### plotting_variables.R 

Allows investigation of TARN dataset. Also splits the PAM and CLARA objects into clusters to facilitate visualisation.
* **Input:** Raw TARN data (79 variables), Pre-pocessed and filtered data (86 variables), Clustering data (21 variables), PAM object or CLARA object
* **Output:** PAM and CLARA objects split into lists, visualisation of the total data or data structure per cluster

## Results

### pam_plots

Folder of average silhouette width (ASW) plots for k-optimisation and ASW per cluster and variable importance plots from each round of PAM sampling.

## Other

*This section briefly introduces some other techniques that were attempted but were not part of the final report.*

### ordinal_pca.R

Dimensionality reduction technique for mixed type data attempted for data exploration
* **Input:** Raw TARN data (79 variables), Pre-pocessed and filtered data (86 variables), Clustering data (21 variables)
* **Output:** Ordinal PCA object and respective plots

### multiple_factor_analysis.R

Multiple factor analysis for mixed type data attempted for data exploration
* **Input:** Raw TARN data (79 variables), Pre-pocessed and filtered data (86 variables), Clustering data (21 variables)
* **Output:** MFA object and respective plots

