#' Title
#'
#' @param raster_path
#' @param coord_label
#' @param output_path
#'
#' @return The newly created function
#' @export
#'
#' @examples


combine_raster_vector <- function(raster_path, coord_label, output_path = NULL) {
  library(terra)
  library(sf)
  lines <- readLines(coord_label)
  start_line <- grep("^X\\s+Y\\s+Label", lines)

  table_lines <- lines[start_line:length(lines)]
  data <- read.delim2(text = table_lines, header = TRUE, stringsAsFactors = FALSE)
  print(data)

  vector_points <- st_as_sf(data, coords = c("X", "Y"), crs = st_crs(coord_label))

  # Load the raster data
  raster_data <- rast(raster_path)

  # Ensure CRS alignment
  if (st_crs(vector_points) != crs(raster_data)) {
    vector_points <- st_transform(vector_points, crs(raster_data))
  }
}
