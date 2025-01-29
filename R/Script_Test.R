shp_file <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom")
raster_file <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom/08-01-25_enkelboom_stacked.tif")
output_folder <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom/Outputfolder")
txtfile <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom/Outputfolder/insitu_reflectance.txt")


raster_path <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom/08-01-25_enkelboom_stacked.tif")
shapefilepath <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/Enkelboompunte/Enkelboom punte.shp")


regression_train <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/regression_input.txt")

predicted_map_output_path <- ("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/clone/R-Package2025/Example_data_enkelboom/Outputfolder")

# Example Usage:

#1
stack_shp_on_rast(shp_file,raster_file,output_folder)

#2
train_rf_model(txtfile ,response_var = "in_situ")

#3
apply_rf_to_raster(rf, rasterpath, predicted_map_output_path)
