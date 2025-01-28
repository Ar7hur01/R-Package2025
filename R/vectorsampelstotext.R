# Load required libraries
library(sf)

setwd("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask")

# Load the shapefile (replace with your file path)
shapefile <- st_read("Z:/Windows_eagle_local/Geoanalysis/Rpackagetask/Enkelboom shp/ezri shp punte/Enkelboom punte.shp")

# Extract projection (CRS)
projection <- st_crs(shapefile)$proj4string  # Get the projection as PROJ4 string

# Extract coordinates and label
# Assuming "label" is the name of the column for the point labels in your shapefile
point_data <- data.frame(
  X = st_coordinates(shapefile)[, 1],  # Extract X (longitude)
  Y = st_coordinates(shapefile)[, 2],  # Extract Y (latitude)
  Label = shapefile$Label              # Extract labels (replace 'Label' with actual column name)
)

# Save projection to the file
write(projection, file = "label+coord.txt") # First line contains projection information

# Append the point data (coordinates + label)
write.table(
  point_data,
  file = "label+coord.txt",
  append = TRUE,              # Append to the file after projection
  row.names = FALSE,          # Do not write row numbers
  col.names = TRUE,           # Include column headers
  sep = "\t",                 # Use tab-delimited format
  quote = FALSE               # Do not quote strings
)

# Confirm completion
cat("Shapefile successfully converted to text file.\n")

