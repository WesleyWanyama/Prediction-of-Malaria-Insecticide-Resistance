# *****************************************************************************
# PART 1: Resampling Methods ----
#Install and Load the required packages
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

#DATASET (Splitting the dataset)
resistance_dataset <- read_csv("data/resistance_dataset_imputed.csv")

str(resistance_dataset)

# Split the dataset into a 75% training set and a 25% testing set
train_index <- createDataPartition(resistance_dataset$RESISTANCE_INTENSITY,
                                   p = 0.75,
                                   list = FALSE)
resistance_dataset_train <- resistance_dataset[train_index, ]
resistance_dataset_test <- resistance_dataset[-train_index, ]

##Train a Naive Bayes classifier using the training dataset ----
resistance_dataset_model_nb <-
  e1071::naiveBayes(RESISTANCE_INTENSITY ~ .,
                    data = resistance_dataset_train)
#Model summary
print(resistance_dataset_model_nb)
## 3. Test the trained model using the testing dataset ----
### 3.a. Test the trained e1071 Naive Bayes model using the testing dataset ----
predictions_nb_e1071 <-
  predict(resistance_dataset_model_nb,
          resistance_dataset_test[, c("ID", "COUNTRY_NAME", "LATITUDE", "LONGITUDE", "INSECTICIDE_CLASS",
                                     "INSECTICIDE_CONC", "INSECTICIDE_INTENSITY", "INSECTICIDE_TYPE",
                                     "YEAR_START",
                                     "VECTOR_SPECIES",
                                     "STAGE_ORIGIN", "MOSQUITO_NUMBER",
                                     "MORTALITY_ADJUSTED")])
print(predictions_nb_e1071)

#Test results using a confusion matrix ----
print(predictions_nb_e1071)

resistance_dataset_test$RESISTANCE_INTENSITY <- as.factor(resistance_dataset_test$RESISTANCE_INTENSITY)
levels(resistance_dataset_test$RESISTANCE_INTENSITY) <- levels(predictions_nb_e1071)
caret::confusionMatrix(predictions_nb_e1071,
                       resistance_dataset_test[, c("ID", "COUNTRY_NAME", "LATITUDE", "LONGITUDE", "INSECTICIDE_CLASS",
                                                  "INSECTICIDE_CONC", "INSECTICIDE_INTENSITY", "INSECTICIDE_TYPE",
                                                  "YEAR_START",
                                                  "VECTOR_SPECIES",
                                                  "STAGE_ORIGIN", "MOSQUITO_NUMBER",
                                                  "MORTALITY_ADJUSTED", "RESISTANCE_INTENSITY")]$RESISTANCE_INTENSITY)
plot(table(predictions_nb_e1071,
           resistance_dataset_test[, c("ID", "COUNTRY_NAME", "LATITUDE", "LONGITUDE", "INSECTICIDE_CLASS",
                                      "INSECTICIDE_CONC", "INSECTICIDE_INTENSITY", "INSECTICIDE_TYPE",
                                      "YEAR_START",
                                      "VECTOR_SPECIES",
                                      "STAGE_ORIGIN", "MOSQUITO_NUMBER",
                                      "MORTALITY_ADJUSTED", "RESISTANCE_INTENSITY")]$RESISTANCE_INTENSITY))

##Classification: LDA with k-fold Cross Validation ----
###LDA classifier based on a 5-fold cross validation
##Classification: Naive Bayes with Repeated k-fold Cross Validation ----
###Train an e1071::naive Bayes classifier based on the churn variable ----
resistance_dataset_model_nb <-
  e1071::naiveBayes(`RESISTANCE_INTENSITY` ~ ., data = resistance_dataset_train)

###Test the trained naive Bayes classifier using the testing dataset ----
predictions_nb_e1071 <-
  predict(resistance_dataset_model_nb, resistance_dataset_test[, 1:14])

###View a summary of the naive Bayes model and the confusion matrix ----
print(resistance_dataset_model_nb)
confusion_matrix <- caret::confusionMatrix(predictions_nb_e1071, resistance_dataset_test$RESISTANCE_INTENSITY)


# *****************************************************************************
# Evaluation Metrics ----
## pROC ----
if (require("pROC")) {
  require("pROC")
} else {
  install.packages("pROC", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# 1. Accuracy and Cohen's Kappa ----
print(confusion_matrix)

# *****************************************************************************
#Model Performance Comparison ----
sapply(resistance_dataset, class)
sum(is.na(resistance_dataset$RESISTANCE_INTENSITY))
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



train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


### CART ----
set.seed(7)
resistance_model_cart <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset,
                             method = "rpart", trControl = train_control)

### KNN ----
set.seed(7)
resistance_model_knn <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset,
                            method = "knn", trControl = train_control)

### Random Forest ----
set.seed(7)
resistance_model_rf <- train(RESISTANCE_INTENSITY ~ ., data = resistance_dataset,
                           method = "rf", trControl = train_control)

##Call the `resamples` Function ----
# We then create a list of the model results and pass the list as an argument
# to the `resamples` function.

results <- resamples(list(CART = resistance_model_cart, KNN = resistance_model_knn, 
                          RF = resistance_model_rf))

#Display the Results ----
## 1. Table Summary ----
# This is the simplest comparison. It creates a table with one model per row
# and its corresponding evaluation metrics displayed per column.

summary(results)

## 2. Box and Whisker Plot ----
# This is useful for visually observing the spread of the estimated accuracies
# for different algorithms and how they relate.

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)

## 3. Dot Plots ----
# They show both the mean estimated accuracy as well as the 95% confidence
# interval (e.g. the range in which 95% of observed scores fell).

scales <- list(x = list(relation = "free"), y = list(relation = "free"))
dotplot(results, scales = scales)

## 4. Scatter Plot Matrix ----
# This is useful when considering whether the predictions from two
# different algorithms are correlated. If weakly correlated, then they are good
# candidates for being combined in an ensemble prediction.

splom(results)
