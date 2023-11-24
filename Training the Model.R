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

# 2. RMSE, R Squared, and MAE ----

