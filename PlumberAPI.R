# *****************************************************************************
# Plumber API ----
# We can create an API to access the model from outside R using a package
# called Plumber.

# STEP 1. Install and Load the Required Packages ----
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

## Load the Model
loaded_resistance_model_rf <- readRDS("./models/saved_resistance_model_rf.rds")

#* @apiTitle Insecticide Resistance Prediction Model API

#* @apiDescription Used to predict whether malaria vectors are resistant to insecticides.

#* @param arg_id ID.
#* @param arg_country Country where the data was collected (e.g., "Kenya").
#* @param arg_latitude Latitude of the collection site.
#* @param arg_longitude Longitude of the collection site.
#* @param arg_insecticide_class Class of the insecticide used.
#* @param arg_insecticide_concentration Concentration of the insecticide used.
#* @param arg_insecticide_intensity Intensity of insecticide application.
#* @param arg_insecticide_type Type of insecticide used.
#* @param arg_year_start Year when sampling started.
#* @param arg_vector_species Species of mosquitoes.
#* @param arg_stage_origin Stage of mosquito development.
#* @param arg_mosquito_number Number of mosquitoes collected.
#* @param arg_mortality_adjusted Adjusted mortality rate.


#* @get /mosquito_resistance

predict_resistance <- function(arg_id, arg_country, arg_latitude, arg_longitude, arg_insecticide_class, arg_insecticide_concentration,
                               arg_insecticide_intensity, arg_insecticide_type, arg_year_start, arg_vector_species, arg_stage_origin,
                               arg_mosquito_number, arg_mortality_adjusted) {
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(ID = as.character(arg_id),
                                COUNTRY_NAME = as.character(arg_country),
                                LATITUDE = as.numeric(arg_latitude),
                                LONGITUDE = as.numeric(arg_longitude),
                                INSECTICIDE_CLASS = as.character(arg_insecticide_class),
                                INSECTICIDE_CONC = as.numeric(arg_insecticide_concentration),
                                INSECTICIDE_INTENSITY = as.character(arg_insecticide_intensity),
                                INSECTICIDE_TYPE = as.character(arg_insecticide_type),
                                YEAR_START = as.numeric(arg_year_start),
                                VECTOR_SPECIES = as.character(arg_vector_species),
                                STAGE_ORIGIN = as.character(arg_stage_origin),
                                MOSQUITO_NUMBER = as.numeric(arg_mosquito_number),
                                MORTALITY_ADJUSTED = as.numeric(arg_mortality_adjusted))
  # Make a prediction based on the data frame
  predict(loaded_resistance_model_rf, to_be_predicted)
}

