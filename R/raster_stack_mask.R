#function 1 --> combining raster data and shp to a new shp with all attributes

#' Stack and mask of individual raster bands (from Sentinel 2)
#' @param Attention: This function only works with data that has a size smaller than your RAM. If the data is too large, the function will abort R immediately.
#' @param raster_scene_folder: Folder containing the raster files with the bands that will be used for the regression model (either 10/20/60 m Resolution)
#' @param aoi: Area of interest (AOI) as a vector file.
#' @param output_folder: Folder to save the output file (output directory where file will be saved)
#'
#' @returns
#' @export
#'
#' @examples

raster_stack_mask <- function(raster_scene_folder, aoi, output_folder) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("The 'terra' package is required for this function. Please install it.")
  }
  # List all .jp2 files in the specified folder
  all_files <- list.files(path = raster_scene_folder, pattern = "\\.jp2$", full.names = TRUE)

  if (any(is.na(values(aoi)))) {
    aoi <- na.omit(aoi)
  }
  if (any(is.na(values(raster_scene_folder)))) {
    raster_scene_folder <- na.omit(raster_scene_folder)
  }

  # Read in .jp2 files and stack them into a Rasterfile
  raster_stack <- rast(all_files)
  # Read in the AOI (as Vectorfile --> SHP, GeoPackage, GeoJSON or KML
  extent <- vect(aoi)

  # Reproject the shapefile if needed
  if (!identical(crs(extent), crs(raster_stack))) {extent <- project(extent, crs(raster_stack)) }

  # Crop and mask to the polygon extent
  raster_cropped <- crop(raster_stack, extent)

  #masking aborts R immediately cause the stacked tif is too large
  #Function will be excluded from the package
  raster_masked <- mask(raster_cropped, extent)
  print(raster_masked)

  f <- file.path(output_folder, "stacked.tif")
  writeRaster(raster_masked, f, overwrite=TRUE)
}
