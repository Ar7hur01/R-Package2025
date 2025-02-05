#function 1 --> combining raster data and shp to a new shp with all attributes


#' Stack shape file on raster data and mask
#'
#' @param shp_file SHP file with in situ-measurments
#' @param raster_file Raster file with satellite scene from the closest date of the in-situ data acquisition
#' @param output_folder Folder to save the output file (output directory)
#'
#' @returns
#' @export
#'
#' @examples stack_shp_on_rast("in-situ_values.shp", "S1_scene_010125.tif", "output_folder")
stack_shp_on_rast <- function(shp_file, raster_file, output_folder) {
  library(terra)
  shapefile_data <- vect(shp_file)
  raster_data <- rast(raster_file)

  shapefile_reproject <- project(shapefile_data, crs(raster_data))

  extracted_values <- extract(raster_data, shapefile_reproject)
  for (i in seq_along(colnames(extracted_values))) {
    colnames(extracted_values)[i] <- paste0("Band_", i-1)
  }
  # Print renamed dataframe
  print(colnames(extracted_values))

  head(extracted_values)

  shapefile_data <- cbind(shapefile_data, extracted_values[,-1]) # Remove the ID column to avoid duplication
  head(shapefile_data)

  file_name <- "insitu_reflectance.txt"
  file_path <- file.path(output_folder, file_name)

  write.table(
    shapefile_data,
    file = file_path,
    append = FALSE,              # Append to the file after projection
    row.names = FALSE,          # Do not write row numbers
    col.names = TRUE,           # Include column headers
    sep = "\t",                 # Use tab-delimited format
    quote = FALSE,


    )
}
#working perfectly
#stack_shp_on_rast("C:/Users/AD/Desktop/R_Package/Enkelboom/Enkelboom punte.shp",
                  #"C:/Users/AD/Desktop/R_Package/08-01-25_enkelboom_stacked.tif",
                  #"C:/Users/AD/Desktop/R_Package/Output")

