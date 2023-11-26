# *****************************************************************************
#Hyperparameter Tuning ----
#Install and Load the Required Packages ----
## randomForest ----
if (require("randomForest")) {
  require("randomForest")
} else {
  install.packages("randomForest", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## RRF ----
if (require("RRF")) {
  require("RRF")
} else {
  install.packages("RRF", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


#Dataset ----
resistance_dataset <- read_csv("data/resistance_dataset_imputed.csv")
# Remove rows with NAs in the target variable
resistance_dataset <- resistance_dataset[complete.cases(resistance_dataset$RESISTANCE_INTENSITY), ]
unique(resistance_dataset$RESISTANCE_INTENSITY)
resistance_dataset <- resistance_dataset[resistance_dataset$RESISTANCE_INTENSITY != "N/A", ]
# Convert 'INSECTICIDE_CONC' to numeric
resistance_dataset$INSECTICIDE_CONC <- as.numeric(resistance_dataset$INSECTICIDE_CONC)
# Convert target variable to factor 
resistance_dataset$RESISTANCE_INTENSITY <- as.factor(resistance_dataset$RESISTANCE_INTENSITY)
unique(resistance_dataset$RESISTANCE_INTENSITY)
unique(resistance_dataset$STAGE_ORIGIN)
# Exclude rows with 'NR' in the 'STAGE_ORIGIN' variable
resistance_dataset <- subset(resistance_dataset, STAGE_ORIGIN != "NR")
unique(resistance_dataset$RESISTANCE_INTENSITY)
# Remove rows with variations of 'N/A' in the outcome variable
resistance_dataset <- subset(resistance_dataset, 
                             !grepl("N/A", RESISTANCE_INTENSITY, fixed = TRUE))
unique(resistance_dataset$RESISTANCE_INTENSITY)
subset(resistance_dataset, grepl("N/A", RESISTANCE_INTENSITY, fixed = TRUE))
# Reassign levels without 'N/A'
resistance_dataset$RESISTANCE_INTENSITY <- factor(resistance_dataset$RESISTANCE_INTENSITY, levels = levels(resistance_dataset$RESISTANCE_INTENSITY)[-6])
unique(resistance_dataset$RESISTANCE_INTENSITY)
library(caret)

# Identify and remove near-zero variance variables
nzv_vars <- nearZeroVar(resistance_dataset)
resistance_dataset <- resistance_dataset[, -nzv_vars]



resistance_independent_variables <- resistance_dataset[, 1:13]
resistance_dependent_variables <- resistance_dataset[, 14]

#Train the Model ----
seed <- 7
metric <- "Accuracy"

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(seed)
mtry <- sqrt(ncol(resistance_independent_variables))
tunegrid <- expand.grid(.mtry = mtry)
resistance_model_default_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, method = "rf",
                                metric = metric,
                                # enables us to maintain mtry at a constant
                                tuneGrid = tunegrid,
                                trControl = train_control)
print(resistance_model_default_rf)

#Apply a "Random Search" to identify the best parameter value ----
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "random")
set.seed(seed)

resistance_model_random_search_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, method = "rf",
                                      metric = metric,
                                      # enables us to randomly search 12 options
                                      # for the value of mtry
                                      tuneLength = 12,
                                      trControl = train_control)

print(resistance_model_random_search_rf)
plot(resistance_model_random_search_rf)

# Apply a "Manual Search" to identify the best parameter value ----

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 3, search = "random")

tunegrid <- expand.grid(.mtry = c(1:5))

modellist <- list()
for (ntree in c(500, 800, 1000)) {
  set.seed(seed)
  resistance_model_manual_search_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset,
                                        method = "rf", metric = metric,
                                        tuneGrid = tunegrid,
                                        trControl = train_control,
                                        ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- resistance_model_manual_search_rf
}

# Lastly, we compare results to find which parameters gave the highest accuracy
print(modellist)

results <- resamples(modellist)
summary(results)
dotplot(results)


# *****************************************************************************
# Ensemble Methods ----
## caretEnsemble ----
if (require("caretEnsemble")) {
  require("caretEnsemble")
} else {
  install.packages("caretEnsemble", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## C50 ----
if (require("C50")) {
  require("C50")
} else {
  install.packages("C50", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## adabag ----
if (require("adabag")) {
  require("adabag")
} else {
  install.packages("adabag", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# 1. Bagging ----
# Two popular bagging algorithms are:
## 1. Bagged CART
## 2. Random Forest

# Example of Bagging algorithms
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
seed <- 7
metric <- "Accuracy"

## 2.a. Bagged CART ----
set.seed(seed)
resistance_model_bagged_cart <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, method = "treebag",
                               metric = metric,
                               trControl = train_control)

## 2.b. Random Forest ----
set.seed(seed)
resistance_model_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, method = "rf",
                      metric = metric, trControl = train_control)

# Summarize results
bagging_results <-
  resamples(list("Bagged Decision Tree" = resistance_model_bagged_cart,
                 "Random Forest" = resistance_model_rf))

summary(bagging_results)
dotplot(bagging_results)


# *****************************************************************************
#Saving the Model ----

## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# Test the Model ----
# create an 80%/20% data split for training and testing datasets respectively
set.seed(9)
train_index <- createDataPartition(resistance_dataset$RESISTANCE_INTENSITY,
                                   p = 0.80, list = FALSE)
resistance_training <- resistance_dataset[train_index, ]
resistance_testing <- resistance_dataset[-train_index, ]

set.seed(9)
predictions <- predict(resistance_model_default_rf, newdata = resistance_testing)
confusionMatrix(predictions, resistance_testing$RESISTANCE_INTENSITY)


# Save and Load your Model ----
saveRDS(resistance_model_default_rf, "./models/saved_resistance_model_rf.rds")
# The saved model can then be loaded later as follows:
loaded_resistance_model_rf <- readRDS("./models/saved_resistance_model_rf.rds")
print(loaded_resistance_model_rf)

predictions_with_loaded_model <-
  predict(loaded_resistance_model_rf, newdata = resistance_testing)
confusionMatrix(predictions_with_loaded_model, resistance_testing$RESISTANCE_INTENSITY)

