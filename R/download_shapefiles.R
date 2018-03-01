#' Download shape file with tigris and convert it to data.table with option
#' to save as a csv file
#'
#' @export
#'


download_shapefile <- function(state = NULL, geography, year = 2016, ...){
    # download shape file of geography of one whole state,
    # individual county

    path_to_shapefile <- "~/shape_files"

    if (geography == "state"){
        # all in one national file
        shape <- tigris::states(year = year, ...)
        pre_geoid <- "04000US"
    }
    if (geography == "county"){
        # all in one national file
        shape <- tigris::counties(year = year, ...)
        pre_geoid <- "05000US"
    }
    if (geography == "place"){
        shape <- tigris::places(state = state, year = year, ...)
        pre_geoid <- "16000US"
    }
    if (geography == "county subdivision"){
        shape <- tigris::county_subdivisions(state = state, year = year, ...)
        pre_geoid <- "06000US"
    }
    if (geography == "tract") {
        shape <- tigris::tracts(state = state, year = year, ...)
        pre_geoid <- "14000US"
    }
    if (geography == "block group"){
        shape <- tigris::block_groups(state = state, year = year, ...)
        pre_geoid <- "15000US"
    }
    if (geography == "zip code"){
        shape <- tigris::zctas(year = year, ...)
        pre_geoid <- "86000US"   # census data from US file
    }
    if (geography == "school district"){
        shape <- tigris::school_districts(state = state, year = year, ...)
        pre_geoid <- "97000US"   # for unified school district
    }

    # get data in data.table format
    dt <- broom::tidy(shape) %>% setDT()

    n <- length(shape$GEOID)
    others <- data.table(
        id = unique(dt$id),
        GEOID = paste0(pre_geoid, shape$GEOID),  # consistent with census geoid
        STATE = ifelse(rep(is.null(shape$STATEFP), n), NA, shape$STATEFP),
        COUNTY = ifelse(rep(is.null(shape$COUNTYFP), n), NA, shape$COUNTYFP),
        NAME = ifelse(rep(is.null(shape$NAME), n), NA, shape$NAME),
        INTPTLON = as.numeric(shape$INTPTLON),
        INTPTLAT = as.numeric(shape$INTPTLAT)
    )

    combined <- others[dt, on = .(id)] %>%
        # make group to deal with multiple states
        .[, group := paste0(group, "_", STATE)] %>%
        .[, state := fips2name_state(STATE, "state")] %>%
        .[, county := fips2names_county(state, COUNTY)] %>%
        .[, state := fips2name_state(STATE, "abbr")]


    # save as csv file for all counties and for individual counties
    if (!dir.exists(path_to_shapefile)){
        dir.create(path_to_shapefile)
    }

    # save for all all state
    if (is.null(state)) state = "all_states"
    file_name <- paste0(path_to_shapefile, "/",
                        tolower(str_replace(geography, " ", "_")),
                        "_", state, "_", year, "_all_counties.csv")
    fwrite(combined, file = file_name)

    # save for each individual county
    all_state <- combined[, unique(state)]
    for (st in all_state){
        all_county <- combined[state == st, unique(county)]
        if (sum(is.na(all_county)) == 0){
            for (ct in all_county) {
                dt_county <- combined[state == st & county == ct]
                file_name <- paste0(path_to_shapefile, "/",
                                    tolower(str_replace(geography, " ", "_")),
                                    "_", st, "_", year, "_", ct, ".csv")
                fwrite(dt_county, file = file_name)

                cat(paste0("saving ", file_name, "\n"))
            }
        }
    }


    return(combined)
}
