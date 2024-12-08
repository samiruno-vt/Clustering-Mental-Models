# Clustering-Mental-Models

Code for the manuscript "Birds of a Feather: Clustering Mental Models to Explore How
People Think Alike"

Code in this repository:

1) MentalModelsToMatrices.ipynb
2) ClusteringMentalModels.R
3) ClusteringMentalModels_Regression.R

1) MentalModelsToMatrices.ipynb
   Contains the code that takes SD Bot output as input and creates standardized matrices for all participants.
   One must enter their own OpenAI API key for the code to work.
   One must also save/download the resulting matrices as .csv files for the next step in the analysis process.

2) ClusteringMentalModels.R
   Analyzes the data from 1) using the novel approach developed in the manuscript.
   Also saves a dataset to use for regression in 3).

3) ClusteringMentalModels_Regression.R
   Builds regression models to look for evidence of whether the approach in 2) is valid.

