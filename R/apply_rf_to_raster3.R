
## function 3
######### Prediction map function

# Function to apply the trained Random Forest model on new raster data
apply_rf_to_raster <- function(rf_model, raster_path, output_raster_path) {
  # Load the satellite raster (input data)
  satellite_raster <- rast(raster_path)  # Use 'rast' from the terra package

  # Extract pixel values (reflectance values) from the raster
  reflectance_values <- as.data.frame(satellite_raster, xy = TRUE)

  # Remove NA values from the reflectance data (if necessary)
  reflectance_values <- na.omit(reflectance_values)
head(reflectance_values)
  # Apply the trained Random Forest model to the reflectance values (predict the response variable)
  predictions <- predict(rf, newdata = reflectance_values[, -c(1, 2)])  # Exclude x, y columns for prediction

  # Convert the predictions into a raster format
  predicted_raster <- satellite_raster
  values(predicted_raster) <- predictions

  # Plot the regression map (predicted values)
  plot(predicted_raster, main = "Regression Map: Predicted Values")

  # Save the regression map as a new .tiff file
  writeRaster(predicted_raster, output_raster_path, format = "GTiff", overwrite = TRUE)
}

