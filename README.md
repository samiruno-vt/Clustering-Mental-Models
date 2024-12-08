# Clustering-Mental-Models

Code for the manuscript "Birds of a Feather: Clustering Mental Models to Explore How
People Think Alike"

Code in this repository:

1) MentalModelsToMatrices.ipynb
2) ClusteringMentalModels.R
3) ClusteringMentalModels_Regression.R

<br />
<br />
MentalModelsToMatrices.ipynb<br />
   &emsp;Contains the code that takes SD Bot output as input and creates standardized matrices for all participants.<br />
   &emsp;One must enter their own OpenAI API key for the code to work.<br />
   &emsp;One must also save/download the resulting matrices as .csv files for the next step in the analysis process.

<br />
<br />
ClusteringMentalModels.R<br />
   &emsp;Analyzes the data from 1) using the novel approach developed in the manuscript.<br />
   &emsp;Also saves a dataset to use for regression in 3).

<br />
<br />
ClusteringMentalModels_Regression.R<br />
   &emsp;Builds regression models to look for evidence of whether the approach in 2) is valid.

