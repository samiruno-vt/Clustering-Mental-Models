

### 44 people's mental models, Matrify threshold = 0.85


library(tidyverse)
library(gt)
library(readxl)
library(cluster)
library(openxlsx)



### Read in the data (matrices are in .csv files output by Matrify
# extension that turns SD Bot output into standardized matrices

# Set the directory where your CSV files are located
csv_dir <- "(directory with .csv files)"

# Define the pattern for matching files
file_pattern <- "^person\\d+\\.csv$"

# List all CSV files in the directory
csv_files <- list.files(csv_dir, pattern = file_pattern, full.names = TRUE)

# Initialize an empty list to store matrices
matrix_list <- list()

# Loop through each CSV file, read it, and store it as a matrix in the list
for (csv_file in csv_files) {
  
  # Read the CSV file into a data frame
  data_frame <- read.csv(csv_file, row.names = 1, check.names = FALSE)
  
  # Merge some additional similar variables that the Matrify process didn't get
  
  #
  # Merge "chat gpt" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["chat gpt", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "chat gpt", ]
  #
  # Merge "ai audited spacial learning" and "special needs education" rows
  data_frame["special needs education", ] <- data_frame["ai audited spacial learning", ] + data_frame["special needs education", ]
  data_frame <- data_frame[rownames(data_frame) != "ai audited spacial learning", ]
  #
  # Merge "ai use" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["ai use", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "ai use", ]
  #
  # Merge "avoiding poor information" and "verification" rows
  data_frame["verification", ] <- data_frame["avoiding poor information", ] + data_frame["verification", ]
  data_frame <- data_frame[rownames(data_frame) != "avoiding poor information", ]
  #
  # Merge "brain usage" and "independent thinking" rows
  data_frame["independent thinking", ] <- data_frame["brain usage", ] + data_frame["independent thinking", ]
  data_frame <- data_frame[rownames(data_frame) != "brain usage", ]
  #
  # Merge "writing jobs" and "certain jobs" rows
  data_frame["certain jobs", ] <- data_frame["writing jobs", ] + data_frame["certain jobs", ]
  data_frame <- data_frame[rownames(data_frame) != "writing jobs", ]
  #
  # Merge "chat bot ai" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["chat bot ai", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "chat bot ai", ]
  #
  # Merge "chatgpt assistance" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["chatgpt assistance", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "chatgpt assistance", ]
  #
  # Merge "chatgpt misuse" and "unethical behavior" rows
  data_frame["unethical behavior", ] <- data_frame["chatgpt misuse", ] + data_frame["unethical behavior", ]
  data_frame <- data_frame[rownames(data_frame) != "chatgpt misuse", ]
  #
  # Merge "reliance on software" and "chatgpt reliance" rows
  data_frame["chatgpt reliance", ] <- data_frame["reliance on software", ] + data_frame["chatgpt reliance", ]
  data_frame <- data_frame[rownames(data_frame) != "reliance on software", ]
  #
  # Merge "chatgpt use" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["chatgpt use", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "chatgpt use", ]
  #
  # Merge "cheating and falsification" and "unethical behavior" rows
  data_frame["unethical behavior", ] <- data_frame["cheating and falsification", ] + data_frame["unethical behavior", ]
  data_frame <- data_frame[rownames(data_frame) != "cheating and falsification", ]
  #
  # Merge "original ideas" and "independent thinking" rows
  data_frame["independent thinking", ] <- data_frame["original ideas", ] + data_frame["independent thinking", ]
  data_frame <- data_frame[rownames(data_frame) != "original ideas", ]
  #
  # Merge "plagiarism" and "unethical behavior" rows
  data_frame["unethical behavior", ] <- data_frame["plagiarism", ] + data_frame["unethical behavior", ]
  data_frame <- data_frame[rownames(data_frame) != "plagiarism", ]
  #
  # Merge "use for tasks" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["use for tasks", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "use for tasks", ]
  #
  # Merge "generative ai use" and "chatgpt" rows
  data_frame["chatgpt", ] <- data_frame["generative ai use", ] + data_frame["chatgpt", ]
  data_frame <- data_frame[rownames(data_frame) != "generative ai use", ]
  #
  # Merge "human creativity" and "creativity" rows
  data_frame["creativity", ] <- data_frame["human creativity", ] + data_frame["creativity", ]
  data_frame <- data_frame[rownames(data_frame) != "human creativity", ]
  #
  # Merge "new jobs" and "jobs" rows
  data_frame["jobs", ] <- data_frame["new jobs", ] + data_frame["jobs", ]
  data_frame <- data_frame[rownames(data_frame) != "new jobs", ]
  #
  # Merge "job opportunities" and "jobs" rows
  data_frame["jobs", ] <- data_frame["job opportunities", ] + data_frame["jobs", ]
  data_frame <- data_frame[rownames(data_frame) != "job opportunities", ]
  #
  # Merge "job efficiency" and "efficiency" rows
  data_frame["efficiency", ] <- data_frame["job efficiency", ] + data_frame["efficiency", ]
  data_frame <- data_frame[rownames(data_frame) != "job efficiency", ]
  #
  # Merge "learning for blind children" and "special needs education" rows
  data_frame["special needs education", ] <- data_frame["learning for blind children", ] + data_frame["special needs education", ]
  data_frame <- data_frame[rownames(data_frame) != "learning for blind children", ]
  
  
  # Merge "chat gpt" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "chat gpt"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "chat gpt"]
  #
  # Merge "ai audited spacial learning" and "special needs education" columns
  data_frame[, "special needs education"] <- data_frame[, "ai audited spacial learning"] + data_frame[, "special needs education"]
  data_frame <- data_frame[, colnames(data_frame) != "ai audited spacial learning"]
  #
  # Merge "ai use" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "ai use"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "ai use"]
  #
  # Merge "avoiding poor information" and "verification" columns
  data_frame[, "verification"] <- data_frame[, "avoiding poor information"] + data_frame[, "verification"]
  data_frame <- data_frame[, colnames(data_frame) != "avoiding poor information"]
  #
  # Merge "brain usage" and "independent thinking" columns
  data_frame[, "independent thinking"] <- data_frame[, "brain usage"] + data_frame[, "independent thinking"]
  data_frame <- data_frame[, colnames(data_frame) != "brain usage"]
  #
  # Merge "writing jobs" and "certain jobs" columns
  data_frame[, "certain jobs"] <- data_frame[, "writing jobs"] + data_frame[, "certain jobs"]
  data_frame <- data_frame[, colnames(data_frame) != "writing jobs"]
  #
  # Merge "chat bot ai" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "chat bot ai"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "chat bot ai"]
  #
  # Merge "chatgpt assistance" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "chatgpt assistance"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "chatgpt assistance"]
  #
  # Merge "chatgpt misuse" and "unethical behavior" columns
  data_frame[, "unethical behavior"] <- data_frame[, "chatgpt misuse"] + data_frame[, "unethical behavior"]
  data_frame <- data_frame[, colnames(data_frame) != "chatgpt misuse"]
  #
  # Merge "reliance on software" and "chatgpt reliance" columns
  data_frame[, "chatgpt reliance"] <- data_frame[, "reliance on software"] + data_frame[, "chatgpt reliance"]
  data_frame <- data_frame[, colnames(data_frame) != "reliance on software"]
  #
  # Merge "chatgpt use" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "chatgpt use"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "chatgpt use"]
  #
  # Merge "cheating and falsification" and "unethical behavior" columns
  data_frame[, "unethical behavior"] <- data_frame[, "cheating and falsification"] + data_frame[, "unethical behavior"]
  data_frame <- data_frame[, colnames(data_frame) != "cheating and falsification"]
  #
  # Merge "original ideas" and "independent thinking" columns
  data_frame[, "independent thinking"] <- data_frame[, "original ideas"] + data_frame[, "independent thinking"]
  data_frame <- data_frame[, colnames(data_frame) != "original ideas"]
  #
  # Merge "plagiarism" and "unethical behavior" columns
  data_frame[, "unethical behavior"] <- data_frame[, "plagiarism"] + data_frame[, "unethical behavior"]
  data_frame <- data_frame[, colnames(data_frame) != "plagiarism"]
  #
  # Merge "use for tasks" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "use for tasks"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "use for tasks"]
  #
  # Merge "generative ai use" and "chatgpt" columns
  data_frame[, "chatgpt"] <- data_frame[, "generative ai use"] + data_frame[, "chatgpt"]
  data_frame <- data_frame[, colnames(data_frame) != "generative ai use"]
  #
  # Merge "human creativity" and "creativity" columns
  data_frame[, "creativity"] <- data_frame[, "human creativity"] + data_frame[, "creativity"]
  data_frame <- data_frame[, colnames(data_frame) != "human creativity"]
  #
  # Merge "new jobs" and "jobs" columns
  data_frame[, "jobs"] <- data_frame[, "new jobs"] + data_frame[, "jobs"]
  data_frame <- data_frame[, colnames(data_frame) != "new jobs"]
  #
  # Merge "job opportunities" and "jobs" columns
  data_frame[, "jobs"] <- data_frame[, "job opportunities"] + data_frame[, "jobs"]
  data_frame <- data_frame[, colnames(data_frame) != "job opportunities"]
  #
  # Merge "job efficiency" and "efficiency" columns
  data_frame[, "efficiency"] <- data_frame[, "job efficiency"] + data_frame[, "efficiency"]
  data_frame <- data_frame[, colnames(data_frame) != "job efficiency"]
  #
  # Merge "learning for blind children" and "special needs education" columns
  data_frame[, "special needs education"] <- data_frame[, "learning for blind children"] + data_frame[, "special needs education"]
  data_frame <- data_frame[, colnames(data_frame) != "learning for blind children"]
  #
  
  # Convert the data frame to a matrix
  matrix_data <- as.matrix(data_frame)
  
  # Store the matrix in the list with the name of the file
  matrix_list[[basename(csv_file)]] <- matrix_data
  
}

rm(matrix_data, data_frame, csv_dir, csv_file, csv_files, file_pattern)




## Collective Matrix
# Summing up all matrices in the list
collective_matrix <- Reduce(`+`, matrix_list)
collective_df <- as.data.frame(collective_matrix)
write.xlsx(collective_df,
           file = "(choose a folder in which to save)/collective_matrix_T85.xlsx",
           colNames = TRUE,
           rowNames = TRUE)

## Average Matrix
average_matrix <- collective_matrix / length(matrix_list)
average_df <- as.data.frame(average_matrix)
write.xlsx(average_matrix,
           file = "(choose a folder in which to save)/average_matrix_T85.xlsx",
           colNames = TRUE,
           rowNames = TRUE)



## Calculate Distances and the Results/Adjacency/Distance Matrix
# pairwise distances between each matrix and store the results in an adjacency matrix (aka distance matrix)
# Matrices are stored in a list called matrix_list

# Function to compute the distance
matrix_distance <- function(M1, M2) {
  return(sum(abs(M1 - M2)))
}

# Compute pairwise distances
# Initialize a matrix to store distances
n <- length(matrix_list)
distances <- matrix(0, n, n)

# Set the row and column names to be the names of the matrices
rownames(distances) <- names(matrix_list)
colnames(distances) <- names(matrix_list)
rownames(distances) <- sub(".csv", "", rownames(distances))
colnames(distances) <- sub(".csv", "", colnames(distances))

for (i in 1:(n-1)) {  # go until the second to last element
  for (j in (i+1):n) {  # to avoid repeated combinations
    d <- matrix_distance(matrix_list[[i]], matrix_list[[j]])
    distances[i, j] <- d
    distances[j, i] <- d  # since the distance is symmetric
  }
}

rm(d, i, j, n)

# # Print the distances
# cat("\nAdjacency Matrix (pairwise distances)\n")
# cat("\n")
# print(distances)



### Network graph/representation of distances between cognitive maps
# network graph with nodes and edges.
# nodes = persons, edges connecting nodes represent the distances between those persons' mental models.

library(igraph)

distances_for_graph <- distances
rownames(distances_for_graph) <- sub("person", "", rownames(distances_for_graph))
colnames(distances_for_graph) <- sub("person", "", colnames(distances_for_graph))


g <- graph_from_adjacency_matrix(
  distances_for_graph,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

# Automatic edge labeling based on weights
E(g)$label <- round(E(g)$weight, 0)

# Plot an initial graph with Kamada-Kawai layout algorithm
plot(g, layout = layout_with_kk(g, weights = E(g)$weight),
     edge.color = 'lightgray',edge.label = NA)



### Community Detection
# We have a weighted, fully-connected network.
# We can use the cluster_louvain function following this paper: https://arxiv.org/abs/0803.0476
# Vincent D. Blondel, Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre: Fast unfolding of communities in large networks. J. Stat. Mech. (2008) P10008
# Also see https://igraph.org/r/doc/cluster_louvain.html

# Note: A larger edge weight means a stronger connection for this function.
# therefore we should use the inverse of the distances matrix

similarities <- 1/(distances_for_graph+0.000001)

similarity_graph <- graph_from_adjacency_matrix(
  similarities,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)


### Detect communities / clusters

communities <- cluster_louvain(similarity_graph, resolution = 1)

sizes(communities)

community_memberships <- membership(communities)

# Create a data frame with IDs and their corresponding cluster memberships
cluster_df <- data.frame(
  person = V(similarity_graph)$name,
  community_membership = community_memberships
) %>%
  mutate(
    person = paste0("person", person),
    community_membership = paste0("cd", as.character(community_membership))
  )




# Read in validation data

vd <- read_xlsx("(directory with all data collected)")

people <- names(matrix_list)
people <- sub("Person", "person", people)
people <- sub(".csv", "", people)

vd <- vd %>%
  filter(
    person %in% people
  ) %>%
  select(
    -ResponseID, ResponseStatus, Response
  ) %>%
  mutate(
    ImportanceEthics = case_when(
      ImportanceEthics == "Not Important" ~ 0,
      ImportanceEthics == "Important" ~ 1,
      ImportanceEthics == "Very Important" ~ 2
    ),
    ImportanceApplications = case_when(
      ImportanceApplications == "Not Important" ~ 0,
      ImportanceApplications == "Important" ~ 1,
      ImportanceApplications == "Very Important" ~ 2
    ),
    ImportanceReliability = case_when(
      ImportanceReliability == "Not Important" ~ 0,
      ImportanceReliability == "Important" ~ 1,
      ImportanceReliability == "Very Important" ~ 2
    ),
    ImportanceEconomics = case_when(
      ImportanceEconomics == "Not Important" ~ 0,
      ImportanceEconomics == "Important" ~ 1,
      ImportanceEconomics == "Very Important" ~ 2
    ),
    ImportanceAccess = case_when(
      ImportanceAccess == "Not Important" ~ 0,
      ImportanceAccess == "Important" ~ 1,
      ImportanceAccess == "Very Important" ~ 2
    ),
    ImportancePolitics = case_when(
      ImportancePolitics == "Not Important" ~ 0,
      ImportancePolitics == "Important" ~ 1,
      ImportancePolitics == "Very Important" ~ 2
    ),
    CategoryMarket = if_else(is.na(CategoryMarket), 0, 1),
    CategoryEthical = if_else(is.na(CategoryEthical), 0, 1),
    CategoryTech = if_else(is.na(CategoryTech), 0, 1),
    CategoryPolitics = if_else(is.na(CetegoryPolitics), 0, 1),
    CategoryEconomy = if_else(is.na(CategoryEconomy), 0, 1)
  ) %>%
  select(-CetegoryPolitics) %>%
  relocate(CategoryPolitics, .after = CategoryTech) %>%
  mutate(
    Optimistic = case_when(
      Optimistic == "1  (Pessimistic)" ~ 1,
      Optimistic == "2" ~ 2,
      Optimistic == "3  (Neutral)" ~ 3,
      Optimistic == "4" ~ 4,
      Optimistic == "5 (Optimistic)" ~ 5,
      TRUE ~ 3
    ),
    ThreatAI = case_when(
      ThreatAI == "1 (Low)" ~ 1,
      ThreatAI == "2" ~ 2,
      ThreatAI == "3" ~ 3,
      ThreatAI == "4" ~ 4,
      ThreatAI == "5 (High)" ~ 5
    ),
    InterestTech = case_when(
      InterestTech == "1 (Low)" ~ 1,
      InterestTech == "2" ~ 2,
      InterestTech == "3" ~ 3,
      InterestTech == "4" ~ 4,
      InterestTech == "5 (High)" ~ 5,
      is.na(InterestTech) ~ NA_integer_
    ),
    UsedChatGPT = if_else(UsedChatGPT == "Yes", 1, 0),
    CurrentChatGPT = if_else(CurrentChatGPT == "Yes", 1, 0),
    FrequencyChatGPT = case_when(
      OftenChatGPT == "Never - do not currently use" ~ 0,
      OftenChatGPT == "Once or a few times a year" ~ 1,
      OftenChatGPT == "Around once a month" ~ 2,
      OftenChatGPT == "Around once a week" ~ 3,
      OftenChatGPT == "Every day" ~ 4
    ),
    YearsOld = if_else(YearBorn == "Before 1999", 26, 2024-as.numeric(YearBorn), missing = NA_integer_),
    Man = if_else(GenderIdentity == "Man", 1, 0),
    Woman = if_else(GenderIdentity == "Woman", 1, 0),
    AnotherGender = if_else(GenderIdentity == "Another gender identity", 1, 0),
    WorkExperience = case_when(
      WorkExperience == "0 years" ~ 0,
      WorkExperience == "1 year" ~ 1,
      WorkExperience == "2 years" ~ 2,
      WorkExperience == "3 years" ~ 3,
      WorkExperience == "4 years" ~ 4,
      WorkExperience == "5 or more years" ~ 5
    ),
    Senior = if_else(EducationStatus == "Undergraduate student: Senior", 1, 0),
    Junior = if_else(EducationStatus == "Undergraduate student: Junior", 1, 0),
    USCitizen = if_else(USCitizen == "Yes", 1, 0)
  )
  

person_vector <- vd$person



#### Prep and save a dataset for regression ####

regression_df <- left_join(
  vd,
  cluster_df,
  by = "person"
)


write.xlsx(regression_df,
           file = "(choose a folder in which to save)/dataset_for_regression_T85.xlsx",
           colNames = TRUE,
           rowNames = FALSE)



### Network Graph, Figure 3 in paper
# layout
# color coded by community/cluster
# might be helpful:  https://kateto.net/netscix2016.html

library(igraph)

similarities_for_graph <- similarities
rownames(similarities_for_graph) <- sub("person", "", rownames(similarities_for_graph))
colnames(similarities_for_graph) <- sub("person", "", colnames(similarities_for_graph))


g <- graph_from_adjacency_matrix(
  similarities_for_graph,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)


# Automatic edge labeling based on weights
E(g)$label <- round(E(g)$weight, 0)


# add community membership to graph (for color coding)
V(g)$community <- cluster_df$community_membership


edge_attr(g)
vertex_attr(g)


set.seed(2824)

# Plot the graph with Kamada-Kawai layout algorithm, try other algorithms
plot(
  g, 
  layout = layout_with_kk(g, weights = E(g)$weight, dim = 2),   # contender
  # layout = layout_nicely(g, weights = E(g)$weight, dim = 3),
  # layout = layout_randomly(g, dim = 2),
  # layout = layout_with_mds(g, dim = 2),
  # layout = layout_with_gem(g),
  # layout = layout_with_lgl(g),
  # layout = layout_with_fr(g, dim = 2, weights = E(g)$weight),   # contender
  # layout = layout_as_bipartite(g),
  # layout = layout_with_dh(g),
  # layout = layout_with_graphopt(g),
  # layout = layout_with_sugiyama(g, weights = E(g)$weight),
  # layout = layout_with_(g, dim = 2, weights = E(g)$weight),
  # layout = layout_with_drl(g, dim = 2, weights = E(g)$weight),
  edge.color = 'darkgray',
  edge.label = NA,
  vertex.label.color="white",
  vertex.color=c( "#861F41", "#E5751F")[1+(V(g)$community=="cd1")]
  )


# Plot the graph with only edges that are at or below median length
# Use Fruchterman-Reingold layout

summary(similarities_for_graph)
summary(E(g)$weight)

g_cleaner <- delete.edges(g, which(E(g)$weight <= .2))

set.seed(2824)

plot(
  g_cleaner,
  layout = layout_with_fr(g_cleaner, dim = 2, weights = E(g_cleaner)$weight),
  edge.color = 'darkgray',
  edge.label = NA,
  vertex.label.color="white",
  vertex.color=c( "#861F41", "#E5751F")[1+(V(g)$community=="cd1")]
  )

