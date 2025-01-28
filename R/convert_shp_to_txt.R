#Function 2 --> take created shp and convert to txt-file
library(roxygen2)

#' @param shapefile_path in characters. File path to the shapefile.
#' @param output_dir in characters. Directory path where the new text file will be created.
#' @param label_column in characters. --> Name of the column in the shapefile that contains point labels.
#' @export
#' @examples

convert_shp_to_txt <- function(shapefile_path, output_dir, label_column) {
  # Load required library
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required. Please install it.")
  }
  library(sf)

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created directory:", output_dir, "\n")
  }

  # Create the output file path
  output_file <- file.path(output_dir, "coords_label.txt")

  # Create the new text file (overwrite if it already exists)
  if (file.exists(output_file)) {
    file.remove(output_file) # Remove existing file to start fresh
  }
  file.create(output_file)

  # Load the shapefile
  shapefile <- st_read(shapefile_path, quiet = TRUE)

  # Extract projection (CRS)
  projection <- st_crs(shapefile)$wkt  # Get the projection in WKT format

  # Validate label column
  if (!label_column %in% colnames(shapefile)) {
    stop(paste("Column", label_column, "not found in the shapefile."))
  }

  # Extract coordinates and labels
  point_data <- data.frame(
    X = st_coordinates(shapefile)[, 1],  # Extract X (longitude)
    Y = st_coordinates(shapefile)[, 2],  # Extract Y (latitude)
    Label = shapefile[[label_column]]    # Extract labels
  )

  # Write projection to the file
  write(projection, file = output_file)  # First line contains projection information

  # Append the point data (coordinates + label)
  write.table(
    point_data,
    file = output_file,
    append = TRUE,              # Append to the file after projection
    row.names = FALSE,          # Do not write row numbers
    col.names = TRUE,           # Include column headers
    sep = "\t",                 # Use tab-delimited format
    quote = FALSE               # Do not quote strings
  )

  # Confirm completion
  cat("Shapefile successfully converted to text file at:", output_file, "\n")
}

