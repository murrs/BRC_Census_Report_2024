# Helper Functions

# Count rows in a data frame keyed to reside.zip
tally_zips <- function(census_list) {
  # Currently the column name containing zip codes is 'reside.zip'
  # This will need to get more sophisticated if the field name ever changes
  
  if (!exists(census_list, inherits = TRUE))
    stop("Oops — ", census_list, " is not in your workspace.")
  
  df <- get(census_list, inherits = TRUE)
  
  if (!"reside.zip" %in% names(df)) {
    # Check for case-insensitive match
    zip_col <- names(df)[tolower(names(df)) == "reside.zip"]
    
    if (length(zip_col) == 1) {
      # Rename the matched column to 'reside.zip'
      df <- rename(df, reside.zip = !!zip_col)
    } else {
      stop("Data frame ", census_list, " must contain a column called 'reside.zip'")
    }
  }
  
  df |>
    filter(!is.na(reside.zip)) |>
    count(reside.zip, name = census_list) |>
    rename(ZCTA = reside.zip) |>
    mutate(ZCTA = as.character(ZCTA))
}

# Merge a list of count tibbles onto a reference table
merge_tallies <- function(ref_tbl, count_tbls) {
  reduce(
    names(count_tbls),
    function(acc, nm) left_join(acc, count_tbls[[nm]], by = "ZCTA"),
    .init = ref_tbl
  ) |>
    mutate(
      across(all_of(names(count_tbls)), ~ tidyr::replace_na(.x, 0L))
    ) |>
    filter(
      rowSums(across(all_of(names(count_tbls)))) > 0
    )
}