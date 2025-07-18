# Helper Functions

# Count rows in a data frame keyed to reside.zip
tally_zip_stats <- function(df_name, scale_to) {
  
  if (!exists(df_name, inherits = TRUE))
    stop("Object ", df_name, " is not in your workspace.")
  
  df <- get(df_name, inherits = TRUE)
  
  # deal with the fact that the column is sometimes called "reside.ZIP" :(
  if (!"reside.zip" %in% names(df)) {
    zip_col <- names(df)[tolower(names(df)) == "reside.zip"]
    if (length(zip_col) == 1) df <- rename(df, reside.zip = !!zip_col)
    else stop("Column 'reside.zip' not found in ", df_name)
  }
  
  df$reside.zip <- substr(df$reside.zip, 1, 5)
  
  if (!"weights" %in% names(df)) stop("Column weights not found in ", df_name)
  
  total_weights <- sum(df$weights, na.rm = TRUE)
  
  df |>
    filter(!is.na(reside.zip)) |>
    group_by(reside.zip) |>
    summarise(
      cnt := n(),
      weights := scale_to * sum(weights)/total_weights,
      .groups = "drop"
    ) |>
    rename(ZCTA = reside.zip) |>
    mutate(ZCTA = as.character(ZCTA))
}

# Create a random number of dots within a geometry
# that corresponds to the weighted population estimate
# for sp_df, which is a data frame with a geometry column
# and a weights column.
jitter_dots <- function(sp_df) {
  if (!"weights" %in% names(sp_df)) {
    stop("Column 'weights' not found in input data")
  }
  
  pts_list <- vector("list", nrow(sp_df))
  
  for (i in seq_len(nrow(sp_df))) {
    n <- round(sp_df$weights[i])
    if (n == 0) next
    geom <- st_geometry(sp_df)[[i]]
    pts <- st_sample(geom, size = n, exact = TRUE)
    if (length(pts) > 0) {
      pts_list[[i]] <- pts
    }
  }
  
  pts_all <- do.call(c, pts_list)
  st_sf(geometry = pts_all, crs = st_crs(sp_df))
}
