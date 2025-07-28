library(dplyr)
library(sf)
library(terra)
library(gstat)
library(ggplot2)
library(dplyr)
library(viridis)
library(spatstat.geom)  # for nndist()
library(scales)

intersection_tally <- read_sf("geo_data/brc_intersection_tally.geojson")
brc_centerlines <- read_sf("geo_data/brc_centerlines.geojson")

# Custom pink color scale
custom_palette <- colorRampPalette(c("white", "#EA008B"))

# Plotting function
plot_interp <- function(sf_df, value_col, title, overlay = NULL) {
  # Extract coordinates for plotting
  coords <- st_coordinates(sf_df)
  plot_df <- sf_df %>%
    st_drop_geometry() %>%
    mutate(x = coords[,1], y = coords[,2])
  
  ggplot(plot_df) +
    geom_tile(aes(x = x, y = y, fill = .data[[value_col]])) + theme_void() +
    { if (!is.null(overlay)) geom_sf(data = overlay, color = "gray50", size = 0.1, inherit.aes = FALSE) } +
    scale_fill_gradientn(
      colors = custom_palette(10),
      na.value = "transparent",
      limits = c(0, max(plot_df[[value_col]], na.rm = TRUE)),
      name = "Population"
    ) +
    coord_sf() +
    labs(title = title, x = NULL, y = NULL) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}

## PREPARE DATA

# Summarize weights in census24 by radial + street
census24_summary <- census24 %>%
  group_by(campRadial, campStreet) %>%
  summarize(sum_wt = sum(weights, na.rm = TRUE), .groups = "drop")

# Apply weights to tally
intersection_tally_weighted <- intersection_tally %>%
  left_join(census24_summary, by = c("street1" = "campRadial", "street2" = "campStreet")) %>%
  mutate(
    weighted_pop = num_matches * sum_wt
  )

# Use a projection (better for interpolating distances)
intersection_tally_weighted_utm <- st_transform(intersection_tally_weighted, 26911)  # UTM zone 11N (Nevada)
brc_centerlines_utm <- st_transform(brc_centerlines, st_crs(intersection_tally_weighted_utm))

# Create a polygon to contain the predictions within

# City including man
# brc_grid_boundary <- brc_centerlines_utm %>%
#   st_union() %>%
#   st_convex_hull()

brc_grid_boundary <- brc_centerlines_utm %>%
  st_buffer(100) %>%         # buffer 100m around streets
  st_union() %>%             # combine all buffered lines
  st_make_valid()            # ensure geometry is clean

# Assess point spacing and resolution to ensure grid resolution ≤ half of mean NN
nn_dist <- mean(nndist(st_coordinates(intersection_tally_weighted_utm)))
print(paste("Mean nearest-neighbor distance (m):", round(nn_dist)))
res_m <- 40  # in meters

# Create prediction grid (warning that raster has no values makes sense)
interp_ext <- st_bbox(intersection_tally_weighted_utm)
r_template <- rast(ext(interp_ext), res = res_m, crs = st_crs(intersection_tally_weighted_utm)$wkt)
brc_grid <- st_as_sf(as.points(r_template), coords = c("x", "y"), crs = st_crs(intersection_tally_weighted_utm))
brc_grid <- brc_grid[brc_grid_boundary,] # mask to city streets only
plot(brc_grid$geometry, pch = ".", main = "Prediction Grid")

# IDW ANALYSIS

# IDW interpolation
trend_eq <- weighted_pop ~ 1 # assume no trend
idw_result <- idw(trend_eq, locations = intersection_tally_weighted_utm, newdata = brc_grid, idp = 2)
brc_grid$idw <- idw_result$var1.pred

# Normalize to reflect total pop estimate
brc_grid$est_pop_idw <- brc_grid$idw / sum(brc_grid$idw, na.rm = TRUE) * 72000



plot_interp(brc_grid, "est_pop_idw", "IDW Interpolation", overlay = brc_centerlines_utm)


# KRIGING ANALYSIS

# Compute experimental variogram and fit variogram model
vgm_exp <- variogram(weighted_pop ~ 1, locations = intersection_tally_weighted_utm)
vgm_fit <- fit.variogram(vgm_exp, model = vgm("Exp"))

# Create gstat model with fitted variogram
gstat_model <- gstat(formula = weighted_pop ~ 1, locations = intersection_tally_weighted_utm, model = vgm_fit)

# Perform kriging prediction on grid points (sf object)
krige_result <- predict(gstat_model, newdata = brc_grid)

# Normalize to total = 72000
krige_result <- krige_result %>%
  mutate(est_pop = var1.pred / sum(var1.pred, na.rm = TRUE) * 72000)

# Combine predictions with grid geometry
brc_grid$est_pop_krige <- krige_result$est_pop

# Cross-validation
krige_cv <- krige.cv(weighted_pop ~ 1, locations = intersection_tally_weighted_utm, nfold = 10)

# Summarize CV residuals
cv_summary <- data.frame(
  RMSE = sqrt(mean(krige_cv$residual^2)),
  MAE  = mean(abs(krige_cv$residual)),
  ME   = mean(krige_cv$residual)
)
print(cv_summary)

intersection_tally_weighted_utm$residual <- krige_cv$residual

ggplot(intersection_tally_weighted_utm) +
  geom_sf(aes(color = residual)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Kriging Cross-Validation Residuals", color = "Residual")

plot_interp(brc_grid, "est_pop_krige", "Kriging Interpolation", overlay = brc_centerlines_utm)
