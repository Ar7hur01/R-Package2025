install.packages("roxygen2")
library(roxygen2)
######################################################
#' Convert Shapefile to a Text File
#'
#' This function reads a shapefile, extracts its coordinates, labels, and projection,
#' and saves the information into a text file.
#'
#' @param shapefile_path Character. File path to the shapefile.
#' @param output_file Character. File path for the output text file.
#' @param label_column Character. Name of the column in the shapefile that contains point labels.
#' @return NULL. Writes the output directly to the specified file.
#' @examples
#' \dontrun{
#' convert_shapefile_to_txt("path/to/shapefile.shp", "output/label_coord.txt", "Label")
#' }
#' @export
convert_shapefile_to_txt <- function(shapefile_path, output_file, label_column) {
  # Load required library
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required. Please install it.")
  }
  library(sf)

  # Load the shapefile
  shapefile <- st_read(shapefile_path, quiet = TRUE)

  # Extract projection (CRS)
  projection <- st_crs(shapefile)$proj4string  # Get the projection as PROJ4 string

  # Extract coordinates and label
  if (!label_column %in% colnames(shapefile)) {
    stop(paste("Column", label_column, "not found in the shapefile."))
  }
  point_data <- data.frame(
    X = st_coordinates(shapefile)[, 1],  # Extract X (longitude)
    Y = st_coordinates(shapefile)[, 2],  # Extract Y (latitude)
    Label = shapefile[[label_column]]    # Extract labels
  )

  # Save projection to the file
  write(projection, file = output_file) # First line contains projection information

  # Append the point data (coordinates + label)
  write.table(
    point_data,
    file = output_file,
    append = TRUE,              # Append to the file after projection
    row.names = FALSE,          # Do not write row numbers
    col.names = FALSE,           # Include column headers
    sep = "\t",                 # Use tab-delimited format
    quote = FALSE               # Do not quote strings
  )

  # Confirm completion
  cat("Shapefile successfully converted to text file.\n")
}
