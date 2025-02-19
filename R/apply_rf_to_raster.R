## function 4 Prediction map function
# Function to apply the trained Random Forest model on new raster data
#' Apply created Random Forest Model to new/unseen Raster Data to generate a Prediction-Map
#'
#' @param rf_model Trained Random Forest model (from "train_rf_model"-function)
#' @param raster_path Path to the raster file with reflectance values (.tif)
#' @param output_raster_path Path to save the output raster file with predicted values
#'
#' @returns
#' @export
#'
#' @examples

apply_rf_to_raster <- function(rf_model, raster_path, output_raster_path) {
  # load the satellite-scene
  satellite_raster <- rast(raster_path)  # Use 'rast' from the terra package

  #Reading in the Model
  rf_model <- readRDS(rf_model)
  # Extract pixel values (reflectance values) from the raster
  reflectance_values <- as.data.frame(satellite_raster, xy = TRUE)

  # Check if reflectance_values contains data
  if (nrow(reflectance_values) == 0) {
    stop("Error: No data extracted from raster.")
  }

  # Remove NA values from the reflectance data (if necessary)
  reflect_values <- na.omit(reflectance_values)

  # Rename columns (assuming the first two columns are x and y)
  for (i in 3:ncol(reflectance_values)) {
    colnames(reflectance_values)[i] <- paste0("Band_", i-2)  # Subtract 2 to start numbering from 1
  }

  # Ensure newdata has correct format (exclude x, y)
  prediction_input <- reflectance_values[, -c(1, 2)]
  if (!all(sapply(prediction_input, is.numeric))) {
    stop("Error: Non-numeric values found in reflectance data.")
  }

  # Apply the trained Random Forest model to the reflectance values (predict the response variable)
  predictions <- predict(rf_model, newdata = reflectance_values[, -c(1, 2)])  # Exclude x, y columns for prediction
  print(predictions)

  # Ensure predictions are numeric
  if (!is.numeric(predictions)) {
    stop("Error: Model predictions are not numeric.")
  }

  # Convert the predictions into a raster format
  predicted_raster <- satellite_raster[[1]]                                           # Unsure about that
  values(predicted_raster) <- predictions

  # Plot the regression map (predicted values)
  plot(predicted_raster, main = "Regression Map: Predicted Values")                  #plotting didnt work

  # Save as a TIF with decimal numbers of predictes in-situ measurements
  predict <- file.path(output_raster_path, "prediction.tif")
  writeRaster(predicted_raster, predict, datatype = "FLT4S", overwrite = TRUE)
  return(predicted_raster)
}
