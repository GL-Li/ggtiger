#' Quiet "no visible global function definition for" for variable read from csv
#'
#'
NULL


utils::globalVariables(unique(c(
    ".", "id", "group", "STATE", "state", "county", "COUNTY", "all_state_fips",
    "all_county_fips", "fips", "layer", "na.omit", "GEOID", "abbr", "all_fips",
    "coord_map", "lat", "long", "x", "y", "starts_with", "start", "ZCTA5"
)))
