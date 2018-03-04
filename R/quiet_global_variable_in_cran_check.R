#' Quiet "no visible global function definition for" for variable read from csv
#'
#'
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
# Also include function View from package utils. If imported to package, it
# prevents openning data frame in RStudio but instead in a popup window.

utils::globalVariables(unique(c(
    ".", "id", "group", "STATE", "state", "county", "COUNTY", "all_state_fips",
    "all_county_fips", "fips", "layer", "na.omit", "GEOID", "abbr", "all_fips",
    "coord_map", "lat", "long", "x", "y"
)))
