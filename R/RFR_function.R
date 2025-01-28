# Install necessary package if not already installed
if (!require(randomForest)) {
  install.packages("randomForest")
}
if (!require(terra)) {
  install.packages("terra")
}
if (!require(raster)) {
  install.packages("raster")
}


# Load the required packages
library(randomForest)
library(terra)
library(raster)

# Function for training the Random Forest model
train_rf_model <- function(regression_input_path, response_var, predictors_start_column = 5, ntree = 500, mtry = 3) {
  # Load the dataset
  regression_input <- read.table(regression_input_path, header = TRUE, sep = "\t")
  
  # Exclude rows with NA values
  regression_input <- na.omit(regression_input)
  
  # Define the predictors and the response variable
  predictors <- colnames(regression_input[, predictors_start_column:ncol(regression_input)])
  
  # Create a formula for the model
  formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
  
  # Split the data into training and test sets (80% for training, 20% for testing)
  set.seed(123) # For reproducibility
  train_indices <- sample(1:nrow(regression_input), size = 0.8 * nrow(regression_input))
  train_data <- regression_input[train_indices, ]
  test_data <- regression_input[-train_indices, ]
  
  # Train the Random Forest Regression model
  rf_model <- randomForest(formula, data = train_data, ntree = ntree, mtry = mtry, importance = TRUE)
  
  # Print model summary
  print(rf_model)
  
  # Predict on test data
  predictions <- predict(rf_model, newdata = test_data)
  
  # Calculate RMSE (Root Mean Squared Error)
  actual <- test_data[[response_var]]
  rmse <- sqrt(mean((actual - predictions)^2))
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")
  
  # Return the trained model
  return(rf_model)
}


######### Prediction map function

# Function to apply the trained Random Forest model on new raster data
apply_rf_to_raster <- function(rf_model, raster_path, output_raster_path) {
  # Load the satellite raster (input data)
  satellite_raster <- rast(raster_path)  # Use 'rast' from the terra package
  
  # Extract pixel values (reflectance values) from the raster
  reflectance_values <- as.data.frame(satellite_raster, xy = TRUE)
  
  # Remove NA values from the reflectance data (if necessary)
  reflectance_values <- na.omit(reflectance_values)
  
  # Apply the trained Random Forest model to the reflectance values (predict the response variable)
  predictions <- predict(rf_model, newdata = reflectance_values[, -c(1, 2)])  # Exclude x, y columns for prediction
  
  # Convert the predictions into a raster format
  predicted_raster <- satellite_raster
  values(predicted_raster) <- predictions
  
  # Plot the regression map (predicted values)
  plot(predicted_raster, main = "Regression Map: Predicted Values")
  
  # Save the regression map as a new .tiff file
  writeRaster(predicted_raster, output_raster_path, format = "GTiff", overwrite = TRUE)
}
 