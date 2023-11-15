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
resistance_dataset <- read.csv("data/intensity_concentration.csv")

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
resistance_dataset$MORTALITY_ADJUSTED <- as.numeric(as.character(resistance_dataset$MORTALITY_ADJUSTED))

#resistance_dataset_one_way_anova <- aov(TIME_HOLDING_POSTEXPOSURE ~ MORTALITY_ADJUSTED, data = resistance_dataset)
#summary(resistance_dataset_one_way_anova)


##Univariate Plots
#column_to_plot <- as.numeric(as.character(resistance_dataset[, 21]))
#unique(column_to_plot)
#hist(column_to_plot, main = names(resistance_dataset)[21], breaks = 20, col = "lightblue", na.rm = TRUE)

###QUALITATIVE DATA ANALYSIS
#Installing Required Packages
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")



## ggplot2 - For data visualizations using the Grammar for Graphics package ----
if (!is.element("ggplot2", installed.packages()[, 1])) {
  install.packages("ggplot2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggplot2")

## ggrepel - Additional options for the Grammar for Graphics package ----
if (!is.element("ggrepel", installed.packages()[, 1])) {
  install.packages("ggrepel", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggrepel")

## ggraph - Additional options for the Grammar for Graphics package ----
if (!is.element("ggraph", installed.packages()[, 1])) {
  install.packages("ggraph", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("ggraph")

## tidytext - For text mining ----
if (!is.element("tidytext", installed.packages()[, 1])) {
  install.packages("tidytext", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("tidytext")

## tidyr - To tidy messy data ----
if (!is.element("tidyr", installed.packages()[, 1])) {
  install.packages("tidyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("tidyr")

## widyr - To widen, process, and re-tidy a dataset ----
if (!is.element("widyr", installed.packages()[, 1])) {
  install.packages("widyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("widyr")

## gridExtra - to arrange multiple grid-based plots on a page ----
if (!is.element("gridExtra", installed.packages()[, 1])) {
  install.packages("gridExtra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("gridExtra")

## knitr - for dynamic report generation ----
if (!is.element("knitr", installed.packages()[, 1])) {
  install.packages("knitr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("knitr")

## kableExtra - for nicely formatted output tables ----
if (!is.element("kableExtra", installed.packages()[, 1])) {
  install.packages("kableExtra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("kableExtra")

## circlize - To create a cord diagram or visualization ----
# by Gu et al. (2014)
if (!is.element("circlize", installed.packages()[, 1])) {
  install.packages("circlize", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("circlize")

## memery - For creating data analysis related memes ----
# The memery package generates internet memes that optionally include a
# superimposed inset plot and other atypical features, combining the visual
# impact of an attention-grabbing meme with graphic results of data analysis.
if (!is.element("memery", installed.packages()[, 1])) {
  install.packages("memery", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("memery")

## magick - For image processing in R ----
if (!is.element("magick", installed.packages()[, 1])) {
  install.packages("magick", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("magick")

## yarrr - To create a pirate plot ----
if (!is.element("yarrr", installed.packages()[, 1])) {
  install.packages("yarrr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("yarrr")

## radarchart - To create interactive radar charts using ChartJS ----

if (!is.element("radarchart", installed.packages()[, 1])) {
  install.packages("radarchart", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("radarchart")

## igraph - To create ngram network diagrams ----
if (!is.element("igraph", installed.packages()[, 1])) {
  install.packages("igraph", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("igraph")

## wordcloud2 - For creating wordcloud by using 'wordcloud2.JS ----
if (!is.element("wordcloud2", installed.packages()[, 1])) {
  install.packages("wordcloud2", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("wordcloud2")

#Customize the Visualizations, Tables, and Colour Scheme ----
# The following defines a blue-grey colour scheme for the visualizations:
## shades of blue and shades of grey
blue_grey_colours_11 <- c("#27408E", "#304FAF", "#536CB5", "#6981c7", "#8da0db",
                          "#dde5ec", "#c8c9ca", "#B9BCC2", "#A7AAAF", "#888A8E",
                          "#636569")

blue_grey_colours_6 <- c("#27408E", "#304FAF", "#536CB5",
                         "#B9BCC2", "#A7AAAF", "#888A8E")

blue_grey_colours_4 <- c("#27408E", "#536CB5",
                         "#B9BCC2", "#888A8E")

blue_grey_colours_3 <- c("#6981c7", "#304FAF", "#888A8E")

blue_grey_colours_2 <- c("#27408E",
                         "#888A8E")

blue_grey_colours_1 <- c("#6981c7")

# Custom theme for visualizations
blue_grey_theme <- function() {
  theme(
    axis.ticks = element_line(
      linewidth = 1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    axis.text = element_text(
      face = "bold", color = "#3f3f41",
      size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", color = "#3f3f41",
                              size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", color = "#3f3f41",
                              size = 16, hjust = 0.5),
    panel.grid = element_line(
      linewidth = 0.1, linetype = "dashed",
      lineend = NULL, color = "#dfdede",
      arrow = NULL, inherit.blank = FALSE),
    panel.background = element_rect(fill = "#f3eeee"),
    legend.title = element_text(face = "plain", color = "#3f3f41",
                                size = 12, hjust = 0),
    legend.position = "right"
  )
}

# Customize the text tables for consistency using HTML formatting
kable_theme <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

## Special Characters and Lower Case 
remove_special_characters <- function(doc) {
  gsub("[^a-zA-Z0-9 ]", "", doc, ignore.case = TRUE)
}

resistance_dataset$INSECTICIDE_CONC <- sapply(resistance_dataset$INSECTICIDE_CONC, remove_special_characters)

##Removing letter "g" from insecticide conc
resistance_dataset$INSECTICIDE_CONC <- gsub("g", "", resistance_dataset$INSECTICIDE_CONC)


##Removing the HRS and MINS from Time holding exposure column
resistance_dataset$TIME_HOLDING_POSTEXPOSURE <- as.numeric(gsub("[^0-9.]", "", resistance_dataset$TIME_HOLDING_POSTEXPOSURE))
