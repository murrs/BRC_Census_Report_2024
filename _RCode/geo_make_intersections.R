library(dplyr)
library(sf)
library(ggplot2)

# Create intersections at the intersection of all Black Rock City (BRC) streets, 
# one per junction. Note: you will get this error: 
# `although coordinates are longitude/latitude, st_intersection assumes that they are planar`
# We're ignoring it because BRC is not large enough for this to matter. 
# The resolution would be to use projected (map) coordinates instead of latitude and longitude.

# import street centerlines (n=580)
brc_centerlines <- read_sf("https://bm-innovate.s3.amazonaws.com/2024/GIS/GeoJSON/Street_Lines.geojson")

# display the result
brc_map <- ggplot() + geom_sf(data = brc_centerlines) + theme_void()

# create intersections and addresses (n=1601, with lots of dupes)
intersections <- st_intersection(brc_centerlines) %>%
  filter(st_geometry_type(geometry) == "POINT") %>%
  rowwise() %>%
  mutate(
    # Original list with duplicates
    name_shorts = list(c(name_short, brc_centerlines$name_short[unlist(origins)])),
    
    # Deduplicate, replace "Esp", and sort alphabetically
    streets = list(sort(unique(gsub("^Esp$", "Esplanade", name_shorts)))),
    
    # Assumes there are now only two streets
    street1 = streets[1],
    street2 = ifelse(length(streets) > 1, streets[2], NA),
    
    # Collapse to single string
    address = paste(streets, collapse = " & ")
  ) %>%
  ungroup()

# Add addresses
intersections$street2[intersections$street1 == "12:00"] <- "The Man"

intersections <- intersections %>%
  filter(
    str_count(address, "&") == 1, 
    str_detect(address, "(?i)[a-z0-9:]+\\s*&\\s*[a-z0-9:]+")
  )

# Dedupe, one record per address, geometry
intersections <- intersections %>%
  distinct(address, geometry, .keep_all = TRUE)

# Show the result
brc_map + geom_sf(data = intersections, color = '#EA008B')

# Export data
export_file = "geo_data/brc_intersections.geojson"
save_columns = c("street1", "street2", "address", "geometry")
write_sf(intersections[,save_columns], export_file, driver="GeoJSON") # Note GeoJSON is best for web,
                                                                     # for further analysis use GPKG or SHP
# Save BRC 2024 centerlines
save_file = "geo_data/brc_centerlines.geojson"
write_sf(brc_centerlines, save_file, driver="GeoJSON")