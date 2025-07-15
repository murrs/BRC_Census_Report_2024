# Helper Functions

# Count rows in a data frame keyed to reside.zip
tally_zip_stats <- function(df_name) {
  
  if (!exists(df_name, inherits = TRUE))
    stop("Object ", df_name, " is not in your workspace.")
  
  df <- get(df_name, inherits = TRUE)
  
  # robust zip detection
  if (!"reside.zip" %in% names(df)) {
    zip_col <- names(df)[tolower(names(df)) == "reside.zip"]
    if (length(zip_col) == 1) df <- rename(df, reside.zip = !!zip_col)
    else stop("Column 'reside.zip' not found in ", df_name)
  }
  
  has_wgt <- "weights" %in% names(df)
  
  df |>
    filter(!is.na(reside.zip)) |>
    group_by(reside.zip) |>
    summarise(
      !!paste0(df_name, "_cnt") := n(),
      !!paste0(df_name, "_wgt") := if (has_wgt) sum(weights, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    rename(ZCTA = reside.zip) |>
    mutate(ZCTA = as.character(ZCTA))
}

# Merge a list of count tibbles onto a reference table,
# in this case joining on ZCTA to sum up the counts and weights for
# each census year, but could be expanded / modified
merge_tallies <- function(ref_tbl, stat_tbls) {
  reduce(
    stat_tbls,
    function(acc, tbl) {
      # Sanity check: should only have ZCTA + _cnt + _wgt
      other_cols <- setdiff(names(tbl), "ZCTA")
      if (length(other_cols) != 2 ||
          !any(endsWith(other_cols, "_cnt")) ||
          !any(endsWith(other_cols, "_wgt"))) {
        stop("Each stat_tbl must have exactly one '_cnt' and one '_wgt' column.")
      }
      
      acc <- left_join(acc, tbl, by = "ZCTA")
      
      # Replace NA values with 0 in the newly joined columns
      acc[other_cols] <- lapply(acc[other_cols], tidyr::replace_na, 0L)
      
      acc
    },
    .init = ref_tbl
  ) |>
    filter(
      rowSums(across(ends_with("_cnt") | ends_with("_wgt"))) > 0
    )
}