#Installing the Required Packages
## dplyr ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

## naniar ----
if (!is.element("naniar", installed.packages()[, 1])) {
  install.packages("naniar", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("naniar")

## ggplot2 ----
# We require the "ggplot2" package to create more appealing visualizations
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## MICE ----
# We use the MICE package to perform data imputation
if (!is.element("mice", installed.packages()[, 1])) {
  install.packages("mice", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("mice")

## Amelia ----
if (!is.element("Amelia", installed.packages()[, 1])) {
  install.packages("Amelia", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("Amelia")

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
# Load the caret package
library(caret)

#Load the Dataset
resistance_dataset <- read.csv("data/resistance_dataset_new.csv")
resistance_dataset$MOSQUITO_NUMBER <- as.numeric(as.character(resistance_dataset$MOSQUITO_NUMBER))

#Create subset of Variables/Features
resistance_long_dataset <- resistance_dataset %>%
  select(ID, COUNTRY_NAME, LATITUDE, LONGITUDE, INSECTICIDE_CLASS, INSECTICIDE_CONC, INSECTICIDE_INTENSITY,
         INSECTICIDE_TYPE, YEAR_START, VECTOR_SPECIES, STAGE_ORIGIN, MOSQUITO_NUMBER, MORTALITY_ADJUSTED, RESISTANCE_INTENSITY)

##Select 500 random observations
rand_ind <- sample(seq_len(nrow(resistance_long_dataset)), 500)
resistance_dataset <- resistance_long_dataset[rand_ind, ]

# Confirm the "missingness" in the Dataset before Imputation ----
# Are there missing values in the dataset?
any_na(resistance_dataset)

# How many?
n_miss(resistance_dataset)

# What is the percentage of missing data in the entire dataset?
prop_miss(resistance_dataset)

# How many missing values does each variable have?
resistance_dataset %>% is.na() %>% colSums()

# What is the number and percentage of missing values grouped by
# each variable?
miss_var_summary(resistance_dataset)

# What is the number and percentage of missing values grouped by
# each observation?
miss_case_summary(resistance_dataset)

# Which variables contain the most missing values?
gg_miss_var(resistance_dataset)

# Where are missing values located (the shaded regions in the plot)?
vis_miss(resistance_dataset) + theme(axis.text.x = element_text(angle = 80))

#Use the MICE package to perform data imputation 
somewhat_correlated_variables <- quickpred(resistance_dataset, mincor = 0.3)

resistance_dataset_mice <- mice(resistance_dataset, m = 11, method = "pmm",
                            seed = 7,
                            predictorMatrix = somewhat_correlated_variables)

#Impute the missing data
resistance_dataset_imputed <- mice::complete(resistance_dataset_mice, 1)

#Confirm the missingness in the imputed dataset
miss_var_summary(resistance_dataset_imputed)

# A visual confirmation that the dataset has no more missing values in any
# feature:
Amelia::missmap(resistance_dataset_imputed)

# Are there missing values in the dataset?
any_na(resistance_dataset_imputed)

# How many?
n_miss(resistance_dataset_imputed)

# Which variables contain the most missing values?
gg_miss_var(resistance_dataset_imputed)

#Replacing the missing value in INSECTICIDE_INTENSITY
resistance_dataset_imputed$INSECTICIDE_INTENSITY <- ifelse(
  is.na(resistance_dataset_imputed$INSECTICIDE_INTENSITY),
  "Medium",
  resistance_dataset_imputed$INSECTICIDE_INTENSITY
)

any_na(resistance_dataset_imputed)



# *****************************************************************************
#Exposing the Structure of Data using Data Transforms ----
##Scale Data Transform
### The Scale Basic Transform on the Insecticide Resistance Dataset ----
# BEFORE
summary(resistance_dataset_imputed)
hist(resistance_dataset_imputed[, 6], main = names(resistance_dataset_imputed)[6])
hist(resistance_dataset_imputed[, 12], main = names(resistance_dataset_imputed)[12])
hist(resistance_dataset_imputed[, 13], main = names(resistance_dataset_imputed)[13])

model_of_the_transform <- preProcess(resistance_dataset_imputed, method = c("scale"))
print(model_of_the_transform)
resistance_data_scale_transform <- predict(model_of_the_transform,
                                          resistance_dataset_imputed)

# AFTER
summary(resistance_data_scale_transform)
hist(resistance_data_scale_transform[, 6],
     main = names(resistance_data_scale_transform)[6])
hist(resistance_data_scale_transform[, 12],
     main = names(resistance_data_scale_transform)[12])
hist(resistance_data_scale_transform[, 13],
     main = names(resistance_data_scale_transform)[13])

#Center Data Transform
### The Centre Basic Transform on the Insecticide Resistance Dataset ----
# BEFORE
summary(resistance_dataset_imputed)
boxplot(resistance_dataset_imputed[, 6], main = names(resistance_dataset_imputed)[6])
boxplot(resistance_dataset_imputed[, 12], main = names(resistance_dataset_imputed)[12])
boxplot(resistance_dataset_imputed[, 13], main = names(resistance_dataset_imputed)[13])

model_of_the_transform <- preProcess(resistance_dataset_imputed, method = c("center"))
print(model_of_the_transform)
resistance_data_center_transform <- predict(model_of_the_transform, # nolint
                                           resistance_dataset_imputed)

# AFTER
summary(resistance_data_center_transform)
boxplot(resistance_data_center_transform[, 6],
        main = names(resistance_data_center_transform)[6])
boxplot(resistance_data_center_transform[, 12],
        main = names(resistance_data_center_transform)[12])
boxplot(resistance_data_center_transform[, 13],
        main = names(resistance_data_center_transform)[13])

##Saving the File
write.csv(resistance_dataset_imputed,
          file = "data/resistance_dataset_imputed.csv")

