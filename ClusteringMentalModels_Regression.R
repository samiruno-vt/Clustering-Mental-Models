
### 44 people/matrices
### communities/clusters determined with new approach
### use regression to determine whether approach can be validated using other data
### i.e., if model can predict clusters (better than null)
#         -> evidence supporting new approach


library(readxl)
library(tidyverse)
library(car)
library(corrplot)


# read in dataset saved during first steps

f <- "(folder/directory with data saved for regression)"
setwd(f)
d_raw <- read_xlsx("dataset_for_regression_T85.xlsx")


# prep data

d <- d_raw %>%
  select(
    person,
    community_membership,
    ImportanceEthics:CurrentChatGPT,
    WorkExperience,
    USCitizen,
    FrequencyChatGPT,
    YearsOld,
    Man,
    Woman
  ) %>%
  mutate(
    MemberCommunity2 = if_else(community_membership == "cd1", 0, 1)
  )


# logistic regression models

d <- d %>% dplyr::select(-person, -community_membership)

correlations <- cor(d)
corrplot(correlations)

# model process to find quality models (comment out/include variables to experiment)

model_final <- glm(
  MemberCommunity2 ~
    # ImportanceEthics +
    ImportanceApplications +
    # ImportanceReliability +
    ImportanceEconomics +
    # ImportanceAccess +
    # ImportancePolitics +
    CategoryMarket + 
    # CategoryEthical +
    # CategoryTech +
    # CategoryPolitics +
    # CategoryEconomy +
    # Optimistic +
    # ThreatAI +
    # InterestTech +
    # UsedChatGPT +
    CurrentChatGPT
    # FrequencyChatGPT +
    # WorkExperience +
    # USCitizen +
    # YearsOld +
    # Man +
    , #Woman,
  family = "binomial",
  data = d
)

summary(model_final)

# Null deviance is the deviance for a model with only an intercept
null_deviance_final <- model_final$null.deviance

# Residual deviance is the deviance for the model with predictors
residual_deviance_final <- model_final$deviance

# Degrees of freedom
df_diff_final <- model_final$df.null - model_final$df.residual

# Perform the Chi-square test
chi_square_value_final <- null_deviance_final - residual_deviance_final
p_value_final <- pchisq(chi_square_value_final, df_diff_final, lower.tail = FALSE)

# Output the Chi-square value and p-value
cat("Chi-square for Final Model: ", chi_square_value_final, "\n")
cat("p-value for Final Model: ", p_value_final, "\n")

# If the p-value is less than 0.05,
# then the model with predictors fits the data significantly better
# than the null model (model with no predictors).
# Interpretation
if (any(p_value_final < 0.05)) {
  cat("Success! The final model with predictors fits the data significantly better than the null model (with no predictors).\n")
} else {
  cat("Uh oh. The final model with predictors does not fit the data significantly better than the null model (with no predictors).\n")
}




model_2 <- glm(
  MemberCommunity2 ~
    # ImportanceEthics +
    ImportanceApplications +
    # ImportanceReliability +
    ImportanceEconomics +
    # ImportanceAccess +
    # ImportancePolitics +
    CategoryMarket + 
    CategoryEthical +
    # CategoryTech +
    # CategoryPolitics +
    # CategoryEconomy +
    # Optimistic +
    ThreatAI +
    InterestTech +
    # UsedChatGPT +
    CurrentChatGPT
  # FrequencyChatGPT +
  # WorkExperience +
  # USCitizen +
  # YearsOld +
  # Man +
  , #Woman,
  family = "binomial",
  data = d
)

summary(model_2)

# Null deviance is the deviance for a model with only an intercept
null_deviance_2 <- model_2$null.deviance

# Residual deviance is the deviance for the model with predictors
residual_deviance_2 <- model_2$deviance

# Degrees of freedom
df_diff_2 <- model_2$df.null - model_2$df.residual

# Perform the Chi-square test
chi_square_value_2 <- null_deviance_2 - residual_deviance_2
p_value_2 <- pchisq(chi_square_value_2, df_diff_2, lower.tail = FALSE)

# Output the Chi-square value and p-value
cat("Chi-square for Model 2: ", chi_square_value_2, "\n")
cat("p-value for Model 2: ", p_value_2, "\n")

# If the p-value is less than 0.05,
# then the model with predictors fits the data significantly better
# than the null model (with no predictors).
# Interpretation
if (any(p_value_2 < 0.05)) {
  cat("Success! The final model with predictors fits the data significantly better than the null model (with no predictors).\n")
} else {
  cat("Uh oh. The final model with predictors does not fit the data significantly better than the null model (with no predictors).\n")
}




model_3 <- glm(
  MemberCommunity2 ~
    # ImportanceEthics +
    ImportanceApplications +
    # ImportanceReliability +
    ImportanceEconomics +
    # ImportanceAccess +
    # ImportancePolitics +
    CategoryMarket + 
    CategoryEthical +
    # CategoryTech +
    # CategoryPolitics +
    # CategoryEconomy +
    # Optimistic +
    ThreatAI +
    InterestTech +
    # UsedChatGPT +
    CurrentChatGPT +
  # FrequencyChatGPT +
  WorkExperience +
  USCitizen +
  YearsOld +
  # Man +
  Woman,
  family = "binomial",
  data = d
)

summary(model_3)

# Null deviance is the deviance for a model with only an intercept
null_deviance_3 <- model_3$null.deviance

# Residual deviance is the deviance for the model with predictors
residual_deviance_3 <- model_3$deviance

# Degrees of freedom
df_diff_3 <- model_3$df.null - model_3$df.residual

# Perform the Chi-square test
chi_square_value_3 <- null_deviance_3 - residual_deviance_3
p_value_3 <- pchisq(chi_square_value_3, df_diff_3, lower.tail = FALSE)

# Output the Chi-square value and p-value
cat("Chi-square for Model 3: ", chi_square_value_3, "\n")
cat("p-value for Model 3: ", p_value_3, "\n")

# If the p-value is less than 0.05,
# then the model with predictors fits the data significantly better
# than the null model (with no predictors).
# Interpretation
if (any(p_value_3 < 0.05)) {
  cat("Success! The final model with predictors fits the data significantly better than the null model (with no predictors).\n")
} else {
  cat("Uh oh. The final model with predictors does not fit the data significantly better than the null model (with no predictors).\n")
}





