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
