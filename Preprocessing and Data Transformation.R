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

