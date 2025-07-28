library(dplyr)
library(sf)
library(ggplot2)

# Summarize responses and map to BRC intersections
brc_intersections <- read_sf("geo_data/brc_intersections.geojson")
brc_centerlines <- read_sf("geo_data/brc_centerlines.geojson")

# All possible survey responses for location campStreet and campRadial
validcampStreet <- c(LETTERS[1:12], "Esplanade", 
                     "Rod's Road", "Center Camp Plaza", "DPW Depot", "ESD", "Airport", 
                     "Walk-in camping")
validcampRadial <- paste0(rep(2:10, each = 2), ":", rep(c("00", "30"), times = 9))[1:17]

# Filter down to just the intersections a survey response could map to
brc_intersections_valid <- brc_intersections %>% 
  filter(
    street2 %in% validcampStreet,
    street1 %in% validcampRadial
  )

# Tally responses per intersection
intersection_summary <- brc_intersections_valid %>%
  left_join(census24, by = c("street1" = "campRadial", "street2" = "campStreet")) %>%
  filter(!is.na(weights)) %>%  # Only count actual matches
  group_by(street1, street2, address, geometry) %>%
  summarize(num_matches = n(), .groups = "drop")

# Map the result
brc_map <- ggplot() + geom_sf(data = brc_centerlines) + theme_void()
brc_map + 
  geom_sf(data = intersection_summary, aes(size = num_matches), color = '#EA008B') + 
  scale_size_continuous(range = c(1, 6), name = "Number of Responses") +
  geom_sf_text(data = intersection_summary, aes(label = num_matches), 
               color = "white", size = 2, check_overlap = TRUE) +
  theme_void() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    panel.background = element_rect(fill = "#f5f1e9", color = NA),  # Tan background
    plot.background = element_rect(fill = "#f5f1e9", color = NA),
    legend.background = element_rect(fill = "#f5f1e9", color = NA),
    legend.key = element_rect(fill = "#f5f1e9", color = NA)
  )

# Save summary data
save_summaries_file = "geo_data/brc_intersection_tally.geojson"
write_sf(intersection_summary, save_summaries_file, driver="GeoJSON")

# /etc...
# Further location analysis

census24_campLocation <- census24 %>%
  select(starts_with("camp"))

census24_campLocation %>%
  count(campStreet, campRadial, sort = TRUE) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    percent = paste0(percent, "%"),
    campStreet = as.character(campStreet)
  ) %>%
  mutate(
    campStreet = format(
      campStreet, 
      width = max(nchar(campStreet[!is.na(campStreet)]), na.rm = TRUE)
    ),
    n = format(n, width = 5, justify = "right"),
    percent = format(percent, width = 6, justify = "right")
  ) %>%
  print(row.names = FALSE)

# Completely unmatchable records (103)
census24_campLocation %>% filter( is.na(campStreet) | is.na(campRadial) )

# Special values - these need to have a location assigned or need modification

# Center Camp / Rod's Road / ESD / DPW Depot (81) - best guesses for where to put these:
# -[ ] Center Camp: If the campRadial is 5:30, 6:00 or 6:30 we can put this at B, lots of indecipherables at other radials
# -[ ] Rod's Road: Best I can do is treat this the same as Center Camp
# -[ ] ESD:
# -[ ] DPW Depo: Add a point?
# -[ ] Airport
# -[ ] Walk-in campers
census24_campLocation %>% filter( campStreet %in% c("Center Camp Plaza", "Rod's Road", "DPW Depot", "ESD") ) %>% 
  select ( campStreet, campRadial, campPlaced )

# Walk-in Camping (32)
# ??? -[ ]Add a set of points along the radials
census24_campLocation %>% filter( campStreet %in% c("Walk-in camping") ) %>% 
  select ( campStreet, campRadial, campPlaced )

# Airport (28)
# ??? -[ ] Airport: Add a point?
census24_campLocation %>% filter( campStreet %in% c("Airport") ) %>% 
  select ( campStreet, campRadial, campPlaced )
