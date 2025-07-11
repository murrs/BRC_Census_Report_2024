library(dplyr)
library(sf)
library(ggplot2)

# Double-check that the methodology for deriving intersections works
# by creating buffers around each point.
intersections <- read_sf("geo_data/brc_intersections.geojson")

# Set buffer distance (~10 meters or ~1e-4 degrees)
buffer_dist <- 1e-4

# Create buffer polygons around each intersection point
intersection_buffers <- intersections %>%
  st_buffer(dist = buffer_dist)

# Join intersection points to buffers (polygons) they fall within
intersection_buffers_joined <- st_join(intersection_buffers, 
                                       intersections, 
                                       join = st_intersects, 
                                       suffix = c(".buffer", ""))

# Collapse the buffers and aggregate included addresses into a list
# just to make sure we aren't dropping anything
intersection_buffers_addresses <- intersection_buffers_joined %>%
  group_by(row_id.buffer) %>%
  summarize(
    address = list(unique(address.buffer)),
    all_addresses = list(unique(address)),
    count = n(),
    geometry = first(geometry)
  ) %>%
  ungroup()

ggplot(intersection_buffers_addresses) +
  geom_sf(aes(size = count), color = "pink") +
  scale_size_continuous() +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(size = "Dupes")
