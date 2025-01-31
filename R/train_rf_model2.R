## function 2 for training the Random Forest model
# Install necessary package if not already installed
if (!require(randomForest)) {install.packages("randomForest")}
if (!require(terra)) {install.packages("terra")}
if (!require(raster)) {install.packages("raster")}
# Load the required packages
library(randomForest)
library(terra)
library(raster)

train_rf_model <- function(regression_input_path, response_var, output_folder, ntree = 500, mtry = 3) {
  # Load the dataset
  regression_input <- read.table(regression_input_path, header = TRUE, sep = "\t")

  # Exclude rows with NA values
  regression_input <- na.omit(regression_input)

  # Define the predictors and the response variable
  predictors <- grep("^Band", colnames(regression_input), value = TRUE)
  head(predictors)
  print(predictors)

  # Create a formula for the model
  formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))

  # Split the data into training and test sets (80% for training, 20% for testing)
  set.seed(123) # For reproducibility
  train_indices <- sample(1:nrow(regression_input), size = 0.8 * nrow(regression_input))
  train_data <- regression_input[train_indices, ]
  test_data <- regression_input[-train_indices, ]

  # Train the Random Forest Regression model
  rf_model <- randomForest(formula, data = train_data, ntree = 500, mtry = 3, importance = TRUE)

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
  file_path <- file.path(output_folder, "rf_model.rds")
  saveRDS(rf_model, file_path)
}


