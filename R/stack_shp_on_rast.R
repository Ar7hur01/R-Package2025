#' function 2: Stack shape file on raster data and mask
#'
#' @param shp_file stacked SHP file (your own or the output from function "raster_stack_mask") with in-situ measurements
#' @param raster_file Raster file with satellite scene from the closest date of the in-situ data acquisition (Sentinel-scene from e.g. copernicus-hub)
#' @param output_folder Folder to save the output file (output directory)
#'
#' @returns
#' @export
#'
#' @examples InFieldR$stack_shp_on_rast <- stack_shp_on_rast()
#' data("shp_enkelboom")
#' data("raster_enkelboom")
#' stack_shp_on_rast(shp_enkelboom, raster_enkelboom, tempdir())
#'
#' @source Copernicus hub and in-situ data from a field in Enkelboom, South Africa

stack_shp_on_rast <- function(shp_file, raster_file, output_folder) {
  shapefile_data <- terra::vect(shp_file)
  raster_data <- terra::rast(raster_file)

  shapefile_reproject <- terra::project(shapefile_data, terra::crs(raster_data))

  extracted_values <- terra::extract(raster_data, shapefile_reproject)
  for (i in seq_along(colnames(extracted_values))) {
    colnames(extracted_values)[i] <- paste0("Band_", i-1)
  }

  # Print renamed dataframe for reference
  print(colnames(extracted_values))

  # Combine the extracted values with the shapefile data
  shapefile_data <- cbind(shapefile_data, extracted_values[,-1]) # Remove the ID column to avoid duplication
  #Check-up
  head(shapefile_data)

  file_name <- "insitu_reflectance.txt"
  file_path <- file.path(output_folder, file_name)

  write.table(
    shapefile_data,
    file = file_path,
    append = FALSE,
    row.names = FALSE,
    col.names = TRUE,
    sep = "\t",
    quote = FALSE,
    )
}
