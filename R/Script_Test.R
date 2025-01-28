rasterpath <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/Enkelboompunte/Stacked_S2_Enkelboom_2024_12_29.tif")
shapefilepath <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/Enkelboompunte/Enkelboom punte.shp")



extract_reflectance_data(rasterpath,shapefile)

regression_train <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/regression_input.txt")
satalite_image <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/08-01-25_enkelboom_stacked.tif")
predicted_map_output_path <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/Output")



# Example Usage:
# 1. Train the Random Forest model
rf_model <- train_rf_model(regression_train, response_var = "in_situ")

# 2. Apply the model to new satellite raster data
apply_rf_to_raster(rf_model, satalite_image, predicted_map_output_path)
