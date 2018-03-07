# user functions ===============================================================

#' Download shape file with tigris, convert it to data.tables and save them as
#' csv files.
#'
#' @param state abbreviation of a state, such as "MA".
#' @param geography geography of which shape files to download, such as "county"
#'     and "block group".
#' @param year year of the shape files updated
#' @param ... ... parameters in tigris fucntions such as counties() and block_groups()
#'
#' @return a data.table, and saved csv files
#'
#' @export
#'


download_shapefile <- function(state = NULL, geography, year = 2016, ...){
    # download shape file of geography of one whole state,
    # individual county

    path_to_tiger <- Sys.getenv("PATH_TO_TIGER")

    if (geography == "state"){
        # all in one national file
        dt <- download_states(year = year, ...)
    }
    if (geography == "county"){
        # all in one national file
        dt <- download_counties(year = year, ...)
    }
    if (geography == "place"){
        NULL
    }
    if (geography %in% c("county subdivision", "tract", "block group")){
        dt <- download_others(state = state, geography, year = 2016, ...)
    }
    if (geography == "zip code"){
        dt = download_zipcode(starts_with = starts_with, year = year, ...)
    }
    if (geography == "school district"){
        NULL
    }


    return(dt)
}

#' Download zip code
#'
#' The zip code file is very different from others so need its own download
#' function.
#'
#' @param starts_with The first two digits of zip codes, such as c("02", "99")
#' @param year Year the shape file updated.
#' @param ... ...
#'
#' @export
#'
download_zipcode <- function(starts_with = starts_with, year = year, ...){
    cat("Downloading and unziping ZCTA5 data. Be patient ...")
    options(tigris_use_cache = TRUE)    # needed for zip code data

    shape <- tigris::zctas(starts_with = starts_with, year = year, ...)

    dt <- broom::tidy(shape) %>% setDT()

    others <- data.table(
        id = unique(dt$id),
        GEOID = shape$GEOID10,
        ZCTA5 = shape$ZCTA5CE10
    )

    combined <- others[dt, on = .(id)] %>%
        .[, start := str_extract(ZCTA5, "^.{2}")] %>%
        setnames(c("long", "lat"), c("x", "y"))

    # save for each start
    path_to_tiger <- Sys.getenv("PATH_TO_TIGER")
    if (!dir.exists(path_to_tiger)){
        dir.create(path_to_tiger)
    }
    path_to_zcta <- paste0(path_to_tiger, "/zcta")
    if (!dir.exists(path_to_zcta)){
        dir.create(path_to_zcta)
    }

    # save for each start
    for (st in starts_with){
        dt_st <- combined[start == st] %>%
            .[, .(GEOID, x, y, group)]
        file_name <- paste0(path_to_zcta, "/startwith_", st, "_", year, ".csv")
        cat(paste("Saving", file_name, " ...\n"))
        fwrite(dt_st, file = file_name)
    }

    return(combined[, .(GEOID, x, y, group)])
}


# internal functions ==========================================================
convert_shapefile <- function(shape){
    # get data in data.table format
    dt <- broom::tidy(shape) %>% setDT()

    n <- length(shape$GEOID)
    others <- data.table(
        id = unique(dt$id),
        GEOID = shape$GEOID,  # consistent with census geoid
        STATE = ifelse(rep(is.null(shape$STATEFP), n), NA, shape$STATEFP),
        COUNTY = ifelse(rep(is.null(shape$COUNTYFP), n), NA, shape$COUNTYFP)
    )

    combined <- others[dt, on = .(id)] %>%
        # make group to deal with multiple states
        .[, group := paste0(group, "_", STATE)] %>%
        .[, state := fips2names_state(STATE, "state")] %>%
        .[, county := fips2names_county(state, COUNTY)] %>%
        .[, state := fips2names_state(STATE, "abbr")] %>%
        setnames(c("long", "lat"), c("x", "y"))
}


download_places <- function(){
    NULL
}


download_states <- function(year = 2016, ...){
    # all in one national file
    shape <- tigris::states(year = year, ...)
    combined <- convert_shapefile(shape)

    # save data
    path_to_tiger <- Sys.getenv("PATH_TO_TIGER")
    if (!dir.exists(path_to_tiger)){
        dir.create(path_to_tiger)
    }
    path_to_state <- paste0(path_to_tiger, "/state")
    if (!dir.exists(path_to_state)){
        dir.create(path_to_state)
    }

    states <- combined[, unique(state)]
    for (st in states){
        dt_st <- combined[state == st]  %>%
            # keep only needed for plotting
            .[, .(GEOID, x, y, group)]
        file_name <- paste0(path_to_state, "/", "state_", st, "_", year, ".csv")
        cat(paste0("Saving ", file_name, " ...\n"))
        fwrite(dt_st, file = file_name)
    }

    return(combined[, .(GEOID, state, x, y, group)])
}

download_counties <- function(year = 2016, ...){
    # all in one national file
    shape <- tigris::counties(year = year, ...)
    combined <- convert_shapefile(shape)

    # save data
    path_to_tiger <- Sys.getenv("PATH_TO_TIGER")
    if (!dir.exists(path_to_tiger)){
        dir.create(path_to_tiger)
    }
    path_to_county <- paste0(path_to_tiger, "/county")
    if (!dir.exists(path_to_county)){
        dir.create(path_to_county)
    }

    states <- combined[, unique(state)]
    for (st in states){
        dt_st <- combined[state == st] %>%
            .[, .(GEOID, state, county, x, y, group)]
        file_name <- paste0(path_to_county, "/", "county_", st, "_", year,
                            "_all_counties.csv")
        cat(paste0("Saving ", file_name, " ...\n"))
        fwrite(dt_st, file = file_name)
    }

    return(combined[, .(GEOID, state, county, x, y, group)])
}


download_schooldistrict <- function(){
    NULL
}


download_cbsa <- function(){
    NULL
}


download_others <- function(state = NULL, geography, year = 2016, ...){
    # include county subdivision, tract, and block group. Each state has one
    # file and a state can be further split into counties
    if (geography == "county subdivision"){
        shape <- tigris::county_subdivisions(state = state, year = year, ...)
    }
    if (geography == "tract") {
        shape <- tigris::tracts(state = state, year = year, ...)
    }
    if (geography == "block group"){
        shape <- tigris::block_groups(state = state, year = year, ...)
    }
    combined <- convert_shapefile(shape)

    # save data
    path_to_tiger <- Sys.getenv("PATH_TO_TIGER")
    if (!dir.exists(path_to_tiger)){
        dir.create(path_to_tiger)
    }
    path_to_geography <- paste0(path_to_tiger, "/", str_replace(geography, " ", "_"))
    if (!dir.exists(path_to_geography)){
        dir.create(path_to_geography)
    }

    states <- combined[, unique(state)]
    for (st in states){
        # save for whole state
        dt_st <- combined[state == st] %>%
            .[, .(GEOID, x, y, group)]
        file_name <- paste0(path_to_geography, "/",
                            tolower(str_replace(geography, " ", "_")),
                            "_", st, "_", year, "_", "all_counties.csv")
        fwrite(dt_st, file = file_name)

        # save for each county
        counties <- combined[state == st, unique(county)]
        for (ct in counties) {
            dt_county <- combined[state == st & county == ct] %>%
                .[, .(GEOID, x, y, group)]
            file_name <- paste0(path_to_geography, "/",
                                tolower(str_replace(geography, " ", "_")),
                                "_", st, "_", year, "_", ct, ".csv")
            cat(paste0("saving ", file_name, " ...\n"))
            fwrite(dt_county, file = file_name)
        }
    }

    return(combined[, .(GEOID, state, county, x, y, group)])
}




