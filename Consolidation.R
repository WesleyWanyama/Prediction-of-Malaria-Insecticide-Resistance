# *****************************************************************************
# Plumber API ----
# Install and Load the Required Packages ----
## plumber ----
if (require("plumber")) {
  require("plumber")
} else {
  install.packages("plumber", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# Process a Plumber API ----
# This allows us to process a plumber API
api <- plumber::plumb("PlumberAPI.R")

# Run the API on a specific port ----
# Specify a constant localhost port to use
api$run(host = "127.0.0.1", port = 5022)


# *****************************************************************************
# Consume data from the Plumber API Output (using R) ----
# SInstall and load the required packages ----
## httr ----
if (require("httr")) {
  require("httr")
} else {
  install.packages("httr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## jsonlite ----
if (require("jsonlite")) {
  require("jsonlite")
} else {
  install.packages("jsonlite", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

# Generate the URL required to access the API ----

# We set this as a constant port 5022 running on localhost
base_url <- "http://127.0.0.1:5022/mosquito_resistance"

# We create a named list called "params".
# It contains an element for each parameter we need to specify.
params <- list(arg_country = 'Senegal', arg_latitude = 15.14371, arg_longitude = -12.945,
               arg_insecticide_type = 'Deltamethrin', arg_vector_species = 'An.gambie s.l.', arg_stage_origin = 'F0_ADULTS_(FROM_WILD_LARVAE)',
               arg_mosquito_number = 105, arg_mortality_adjusted = 83, arg_insecticide_concentration = 0.25, arg_insecticide_intensity = 'Medium')

query_url <- httr::modify_url(url = base_url, query = params)

print(query_url)


# Make the request for the model prediction through the API ----
# The results of the model prediction through the API can also be obtained in R
model_prediction <- GET(query_url)

content(model_prediction)

# We can print the specific result as follows:
content(model_prediction)[[1]]

# Parse the response into the right format ----
# We need to extract the results from the default JSON list format into
# a non-list text format:
model_prediction_raw <- content(model_prediction, as = "text",
                                encoding = "utf-8")
jsonlite::fromJSON(model_prediction_raw)

# Enclose everything in a function ----
# All the 3 steps above can be enclosed in a function
get_resistance_predictions <-
  function(arg_country, arg_latitude, arg_latitude,
           arg_insecticide_type, arg_vector_species, arg_stage_origin,
           arg_mosquito_number, arg_mortality_adjusted, arg_insecticide_concentration, arg_insecticide_intensity) {
    base_url <- "http://127.0.0.1:5022/diabetes"
    
    params <- list(arg_country = arg_country, arg_latitude = arg_latitude, arg_longitude = arg_longitude,
                   arg_insecticide_type = arg_insecticide_type, arg_vector_species = arg_vector_species, arg_stage_origin = arg_stage_origin,
                   arg_mosquito_number = arg_mosquito_number, arg_mortality_adjusted = arg_mortality_adjusted, arg_insecticide_concentration = arg_insecticide_concentration, arg_insecticide_intensity = arg_insecticide_intensity)
    
    query_url <- modify_url(url = base_url, query = params)
    
    model_prediction <- GET(query_url)
    
    model_prediction_raw <- content(model_prediction, as = "text",
                                    encoding = "utf-8")
    
    jsonlite::fromJSON(model_prediction_raw)
  }

