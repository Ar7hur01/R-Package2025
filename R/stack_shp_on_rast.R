#function 1 --> combining raster data and shp to a new shp with all attributes


#' Stack and mask of individual raster bands (from Sentinel 2)
#'
#' @param raster_scene_folder: Folder containing the raster files with the bands that will be used for the regression model (either 10/20/60 m Resolution)
#' @param aoi: Area of interest (AOI) as a vector file.
#' @param output_folder: Folder to save the output file (output directory where file will be saved)
#'
#' @returns
#' @export
#'
#' @examples

raster_stack_mask <- function(raster_scene_folder, aoi, output_folder) {
  # List all .jp2 files in the specified folder
  portrait_files <- list.files(path = raster_scene_folder, pattern = "\\.jp2$", full.names = TRUE)

  # Read in .jp2 files and stack them into a Rasterfile
  portrait_stack <- rast(portrait_files)
  # Read in the AOI (as Vectorfile --> SHP, GeoPackage, GeoJSON or KML
  extent <- vect(aoi)

  # Reproject the shapefile if needed
  if (!identical(crs(extent), crs(portrait_stack))) {extent <- project(extent, crs(portrait_stack)) }

  # Crop and mask to the polygon extent
  portrait_cropped <- crop(portrait_stack, extent)
  portrait_masked <- mask(portrait_cropped, extent)
  print(portrait_masked)

  f <- file.path(output_folder, "stacked.tif")
  writeRaster(portrait_masked, f, overwrite=TRUE)
}

#' Stack shape file on raster data and mask
#'
#' @param shp_file SHP file with in situ-measurments
#' @param raster_file Raster file with satellite scene from the closest date of the in-situ data acquisition
#' @param output_folder Folder to save the output file (output directory)
#'
#' @returns
#' @export
#'
#' @examples

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

