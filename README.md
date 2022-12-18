# MVA-Project
Project MVA - MDS 

## The data
- Data set can be found at https://openpowerlifting.gitlab.io/opl-csv/bulk-csv.html
- Metadata can be found at https://openpowerlifting.gitlab.io/opl-csv/bulk-csv-docs.html 
- OG commit: 30eb84bf


## Scripts
All scripts included in this repository have been used to process the data, generate the models and explore the results.

- Preprocessing.r -> First script, where the input data is transformed into the data frame that is going to be used for all other scripts. 

- dataViz.Rmd -> Complementary R Markdown script used to visualize with the data exploration and visualization of the results. Includes the MCA and MFA analysis and their
explorations.

- pca. R -> Exploratory script with all attempts to preform the PCA on the dataset. Contains test with different competition result representations.

- associaton_rules.Rmd -> R Markdown with the execution of the a priori algorithm to find the association rules.

- her-clust-crossVal_MF. R -> Hierarchical clustering exploration. Tests where performed for different sets of variables. Includes validations for the number of clusters selected and small analysis on only Male lifters.

- cluster_profiling.Rmd -> R Markdown used to generate visualizations for cluster profiling and also numerical profiling.

- decision_trees.Rmd -> R Markdown with all decision trees and evaluations performed.
