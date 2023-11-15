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

#Loading the dataset
resistance_dataset <- read.csv("data/intensity_concentration.csv",
                         stringsAsFactors = TRUE)
