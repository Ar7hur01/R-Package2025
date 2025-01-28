# Load necessary libraries
#library(raster)
#library(rgdal)
#library(sf)

# Define the function
extract_reflectance_data <- function(raster_path, shapefile_path, output_csv = NULL) {
  # Step 1: Load the raster file
  raster_file <- raster(raster_path)
  
  # Step 2: Load the shapefile
  shape_file <- st_read(shapefile_path)
  
  # Step 3: Plot the raster and shapefile
  plot(raster_file, main = "Raster with Shapefile Overlay")
  plot(st_geometry(shape_file), add = TRUE, col = "red", pch = 19)
  
  # Step 4: Mask the raster using the shapefile
  masked_raster <- mask(raster_file, as(shape_file, "Spatial"))
  
  # Step 5: Extract reflectance data from the raster at shapefile points
  extracted_data <- extract(raster_file, shape_file)
  
  # Step 6: Combine the extracted data with the shapefile attributes
  extracted_df <- cbind(shape_file, reflectance = extracted_data)
  
  # Step 7: Convert to a dataframe
  extracted_df <- as.data.frame(extracted_df)
  
  # Step 8: Save the dataframe to a CSV file if output_csv is specified
  if (!is.null(output_csv)) {
    write.csv(extracted_df, output_csv, row.names = FALSE)
    cat("Data saved to:", output_csv, "\n")
  }
  
  # Step 9: Return the dataframe
  return(extracted_df)
}

# Example usage
# result <- extract_reflectance_data("path_to_raster_file.tif", "path_to_shapefile.shp", "extracted_data.csv")
