# Install renv:
if (!is.element("renv", installed.packages()[, 1])) {
  install.packages("renv", dependencies = TRUE)
}
require("renv")
renv::init()

#Installing Language Server
if (!is.element("languageserver", installed.packages()[, 1])) {
  install.packages("languageserver", dependencies = TRUE)
}
require("languageserver")

##Loading the dataset
resistance_dataset <- read.csv("data/intensity_concentration.csv",
                         stringsAsFactors = TRUE)

#Dimensions --- The dataset has 1725 observations and 25 variables
dim(resistance_dataset)

#Identifying the data types
sapply(resistance_dataset, class)

##Descriptive Statistics
##Measures of Frequency
resistance_dataset_freq <- resistance_dataset$RESISTANCE_INTENSITY
# Create a frequency table for the 'RESISTANCE_INTENSITY' column
frequency_table <- table(resistance_dataset_freq)
# Calculate the percentage
percentage_table <- prop.table(frequency_table) * 100
# Combine the frequency and percentage tables using cbind
result_table <- cbind(frequency = frequency_table, percentage = percentage_table)
View(result_table)

##Measures of Central Tendency
##Calculate the Mode
resistance_intensity_mode <- names(table(resistance_dataset$RESISTANCE_INTENSITY))[
  which(table(resistance_dataset$RESISTANCE_INTENSITY) == max(table(resistance_dataset$RESISTANCE_INTENSITY)))
]
print(resistance_intensity_mode)

## Measures of Distribution/Dispersion/Spread/Scatter/Variability
###Measure the distribution of the data for each variable
summary(resistance_dataset)

###Measure the standard deviation of each variable ----
selected_columns <- resistance_dataset[, 21]
selected_columns <- ifelse(is.na(selected_columns), 0, selected_columns)  # Replace NA with 0 
selected_columns <- sapply(selected_columns, as.numeric, na.rm = TRUE)
standard_deviations <- sapply(selected_columns, sd)
print(standard_deviations)

###Measure the Kurtosis of each variable
library(moments)

if (!is.element("e1071", installed.packages()[, 1])) {
  install.packages("e1071", dependencies = TRUE)
}
require("e1071")

selected_columns_filtered <- selected_columns[sapply(selected_columns, function(col) sum(!is.na(col)) >= 4)]
kurtosis <- sapply(selected_columns_filtered, kurtosis, type = 2)
print(kurtosis)

##Skewness of each variable
skewness <- sapply(selected_columns, skewness)
print(skewness)

##Inferential Statistics
###ANOVA
#resistance_dataset$TIME_HOLDING_POSTEXPOSURE <- as.numeric(as.character(resistance_dataset$TIME_HOLDING_POSTEXPOSURE))
#resistance_dataset$MORTALITY_ADJUSTED <- as.numeric(as.character(resistance_dataset$MORTALITY_ADJUSTED))

#resistance_dataset_one_way_anova <- aov(TIME_HOLDING_POSTEXPOSURE ~ MORTALITY_ADJUSTED, data = resistance_dataset)
#summary(resistance_dataset_one_way_anova)
