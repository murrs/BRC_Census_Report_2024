# Load required libraries
library(tidyverse)
library(sf)
library(geomtextpath)   # for text along radial lines
library(ggtext)         # optional, for improved label rendering (if needed)
library(ggplot2)        # for mapping
library(ggthemes)       # optional, for improved printing themes
library(showtext)       # optional, for font embedding on print

# Optional: set better print font (if using showtext)
# showtext_auto()  # enable if using showtext

# Pull in all the data
brc_centerlines <- read_sf("geo_data/brc_centerlines.geojson")
geocoded_census_responses <- read_sf("geo_data/brc_intersection_tally.geojson")

# Pull in metadata for labeling
brc_street_label_points <- read_sf("geo_data/brc_street_label_points.geojson")
brc_radial_label_lines <- read_sf("geo_data/brc_radial_label_lines.geojson")
brc_man <- read_sf("geo_data/brc_man.geojson")

# Generate labeling lines for radial streets
# Create lines pointing toward the center or away from it depending on street2
radial_lines <- brc_radial_label_lines |>
  rowwise() |>
  mutate(
    center_x = st_coordinates(brc_man)[1, 1],
    center_y = st_coordinates(brc_man)[1, 2],
    target_x = st_coordinates(geometry)[1],
    target_y = st_coordinates(geometry)[2],
    dx = target_x - center_x,
    dy = target_y - center_y,
    length_mult = if_else(street2 == "Esplanade", -0.5, 0.2),  # - toward, + away from man
    end_x = target_x + length_mult * dx,
    end_y = target_y + length_mult * dy,
    line = st_sfc(
      st_linestring(matrix(c(target_x, target_y, end_x, end_y), ncol = 2, byrow = TRUE)),
      crs = st_crs(brc_radial_label_lines)
    )
  ) |>
  ungroup() |>
  st_as_sf() |>
  mutate(
    line = st_segmentize(line, dfMaxLength = 20),
    line_id = row_number()
  )
st_geometry(radial_lines) <- "line"

# Step 2: Extract coordinates from lines and join back to data
coords <- st_coordinates(radial_lines) |> as_tibble()
radial_text_data <- coords |>
  rename(x = X, y = Y, line_id = L1) |>
  mutate(street1 = radial_lines$street1[line_id])

# Create offsets for the street labels
coords <- st_coordinates(brc_street_label_points)
brc_street_label_points <- brc_street_label_points |>
  mutate(
    x = coords[, 1],
    y = coords[, 2],
    nudge_x = case_when(
      street1 == "2:00" & street2 == "Esp" ~ 0.0002,
      street1 == "2:00" ~ 0.0001,
      street2 == "Esp" ~ 0.0013,
      TRUE ~ 0.0010
    ),
    nudge_y = case_when(
      street1 == "2:00" ~ 0.0010,
      TRUE ~ 0.0001
    ),
    label_x = x + nudge_x,
    label_y = y + nudge_y
  )

# Map the result
brc_map <- ggplot() + geom_sf(data = brc_centerlines) + theme_void() +
  geom_sf(data = geocoded_census_responses, aes(size = num_matches), color = '#EA008B') +
  scale_size_continuous(range = c(3, 8), name = "Number of Responses") +
  geom_sf_text(data = intersection_summary, aes(label = num_matches), 
               color = "white", size = 2.5, check_overlap = FALSE) +
  geom_text(data = brc_street_label_points, aes(x = label_x, y = label_y, label = street2),
            color = "grey30", size = 3, fontface = "bold") +
  geom_textpath(data = radial_text_data, aes(x = x, y = y, label = street1, group = line_id),
                text_only = TRUE, size = 3, color = "grey30") +
  theme(
    legend.position = c(0.9, 0.9),
    legend.justification = c("right", "top")
  ) +
  labs(title = "2024 Census Responses by BRC Address") +
  coord_sf()

# Aggregate num_matches
census_summary <- census24 |>
  mutate(
    radial_valid = campRadial %in% c("5:30", "6:00", "6:30"),
    group_label = case_when(
      campStreet %in% c("Center Camp", "Rod's Road") & !radial_valid ~ "Invalid address",
      campStreet %in% c("Center Camp", "Rod's Road") & radial_valid ~ "Center Camp Plaza",
      campStreet == "Airport" ~ "Airport",
      campStreet == "DPW Depot" ~ "DPW Depot",
      campStreet == "ESD" ~ "ESD",
      campStreet == "Walk-in camping" ~ "Walk-in camping",
      is.na(campStreet) | is.na(campRadial) ~ "No address",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(group_label)) |>
  count(group_label, name = "num_matches")

# Get bounding box of the main map
bbox <- st_bbox(geocoded_census_responses)

# Arrange and define label positions
census_summary <- census_summary |>
  arrange(num_matches) |>
  mutate(
    lat = bbox["ymin"] - 0.005,                             # 0.001 deg south
    lon = bbox["xmin"] + seq(0, by = 0.008, length.out = n())  # in a row
  )

# Convert to sf with same CRS
census_summary_sf <- st_as_sf(census_summary, coords = c("lon", "lat"), crs = 4326)

brc_map_final <- brc_map +
  geom_sf(data = census_summary_sf, aes(size = num_matches), color = "#EA008B") +
  geom_sf_text(data = census_summary_sf, aes(label = num_matches), color = "white", size = 2.5) +
  geom_sf_text(data = census_summary_sf, aes(label = group_label, geometry = geometry), 
               nudge_y = -0.0012, size = 3, color = "grey30")

# Save in print-friendly landscape mode (11 x 8.5 inches)
ggsave("./brc_census_response_map.pdf", brc_map_final, width = 11, height = 8.5, units = "in", dpi = 300)

# Optionally also save a high-resolution PNG version
ggsave("./brc_census_response_map.png", brc_map_final, width = 11, height = 8.5, units = "in", dpi = 300)
