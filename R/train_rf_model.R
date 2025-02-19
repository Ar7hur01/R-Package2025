## function 3 for training the Random Forest model

#' Train Random Forest Regression Model
#' This function trains a Random Forest regression model using the input data and saves the trained model to a file.
#' It will show you the RMSE as output to evaluate the accuracy of your model.
#'
#' @param regression_input_path Path to the input file containing the regression data (Output file from "stack_shp_on_rast")
#' @param response_var Name of the in-situ data column (response-variable) in the regression data
#' @param output_folder Folder to save the trained model
#' @param ntree Number of trees in the Random Forest model
#' @param mtry Number of variables randomly sampled as candidates at each split
#'
#' @returns
#' @export
#'
#' @examples

train_rf_model <- function(regression_input_path, response_var, output_folder, ntree = 500, mtry = 3) {
  # Load the dataset
  regression_input <- read.table(regression_input_path, header = TRUE, sep = "\t")

  # Exclude rows with NA values
  regression_input <- na.omit(regression_input)

  # Define the predictors and the response variable
  predictors <- grep("^Band", colnames(regression_input), value = TRUE)
  head(predictors)

  # Create a formula for the model
  formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))

  # Split the data into training and test sets (80% for training, 20% for testing)
  set.seed(123) # For reproducibility
  train_indices <- sample(1:nrow(regression_input), size = 0.8 * nrow(regression_input))
  train_data <- regression_input[train_indices, ]
  test_data <- regression_input[-train_indices, ]

  # Train the Random Forest Regression model
  rf_model <- randomForest::randomForest(formula, data = train_data, ntree = 500, mtry = 3, importance = TRUE)

  # Print model summary
  print(rf_model)

  # Predict on test data
  predictions <- terra::predict(rf_model, newdata = test_data)

  # Calculate RMSE (Root Mean Squared Error)
  actual <- test_data[[response_var]]
  rmse <- sqrt(mean((actual - predictions)^2))
  cat("Root Mean Squared Error (RMSE):", rmse, "\n")

  file_path <- file.path(output_folder, "rf_model.rds")
  saveRDS(rf_model, file_path)

  # Return the trained model
  return(rf_model)
}
