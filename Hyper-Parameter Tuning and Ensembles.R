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

#Dataset ----
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
# mtry <- sqrt(ncol(sonar_independent_variables))

resistance_model_random_search_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, method = "rf",
                                      metric = metric,
                                      # enables us to randomly search 12 options
                                      # for the value of mtry
                                      tuneLength = 12,
                                      trControl = train_control)

print(resistance_model_random_search_rf)
plot(resistance_model_random_search_rf)

# Apply a "Grid Search" to identify the best parameter value ----
# Each axis of the grid is an algorithm parameter, and points on the grid are
# specific combinations of parameters.

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                              search = "grid")
set.seed(seed)

getModelInfo("RRFglobal")

tunegrid <- expand.grid(.mtry = c(1:10),
                        .coefReg = seq(from = 0.1, to = 1, by = 0.1))

resistance_model_grid_search_rrf_global <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset, # nolint
                                            method = "RRFglobal",
                                            metric = metric,
                                            tuneGrid = tunegrid,
                                            trControl = train_control)
print(resistance_model_grid_search_rrf_global)
plot(resistance_model_grid_search_rrf_global)

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
