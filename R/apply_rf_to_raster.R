## function 4 Prediction map function
#' Function to apply the trained Random Forest model on new raster data
#' Apply created Random Forest Model to new/unseen Raster Data to generate a Prediction-Map. Output can be used in GIS software to look at exact prediction values of each pixel.
#'
#' @param rf_model Trained Random Forest model (from "train_rf_model"-function)
#' @param raster_path Path to the raster file with reflectance values (.tif-file)
#' @param output_raster_path Path to save the output raster file with predicted values
#'
#' @returns
#' @export
#'
#' @examples
#' data("my_raster", package = "InFieldR")  # loads a SpatRaster
#' data("rf_model", package = "InFieldR")   # loads a trained randomForest model
#'
#' # Predict and plot the result
#' result <- apply_rf_to_raster(rf_model, my_raster, tempdir())
#' terra::plot(result, main = "Predicted Raster from Random Forest")


apply_rf_to_raster <- function(rf_model, raster_path, output_raster_path) {
  # load the satellite scene as raster
  satellite_raster <- terra::rast(raster_path)  # Use 'rast' from the terra package
  # convert raster to data frame
  df <- as.data.frame(satellite_raster, xy = TRUE)  # 'xy = TRUE' keeps the coordinates
  # Rename columns (assuming the first two columns are x and y)
  for (i in 3:ncol(df)) {
    colnames(df)[i] <- paste0("Band_", i-2)  # Subtract 2 to start numbering from 1
  }
  # print for rechecking
  print(df)

  # predict using the random forest model on the data frame (= raster data)
  actual_rf_model <- readRDS(rf_model)
  df$predicted <- terra::predict(actual_rf_model, newdata = df)

  # Convert predictions back to a raster
  r_pred <- satellite_raster[[1]]  # Use first band as a template
  terra::values(r_pred) <- df$predicted  # Assign predicted values to the raster

  # Plot the predicted raster
  terra::plot(r_pred, main = "Predicted Raster Output")

  # Save as a TIF with decimal numbers of predicted in-situ measurements
  new_file <- file.path(output_raster_path, "prediction.tif")
  terra::writeRaster(r_pred, new_file, datatype = "FLT4S", overwrite = TRUE)
  return(r_pred)
}
