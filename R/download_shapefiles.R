#' Download shape file with tigris and convert it to data.table with option
#' to save as a csv file
#'
#' @export
#'


download_shapefile <- function(state, geo_header, year = 2017){
    # download shape file of geo_header of selected states
    if (geo_header == "COUNTY"){
        shape <- tigris::counties(state = state, year = year)
        pre_geoid <- "05000US"
    }
    if (geo_header == "COUSUB"){
        shape <- tigris::county_subdivisions(state = state, year = year)
        pre_geoid <- "06000US"
    } else if (geo_header == "TRACT") {
        shape <- tigris::tracts(state = state, year = year)
        pre_geoid <- "14000US"
    }

    # get data in data.table format
    dt <- broom::tidy(shape) %>% setDT

    others <- data.table(
        id = unique(dt$id),
        STATE = shape$STATEFP,
        GEOID = paste0(pre_geoid, shape$GEOID),  # consistent with census geoid
        NAME = shape$NAME,
        INTPTLON = as.numeric(shape$INTPTLON),
        INTPTLAT = as.numeric(shape$INTPTLAT)
    )

    combined <- others[dt, on = .(id)] %>%
        # make group to deal with multiple states
        .[, group := paste0(group, "_", STATE)]

    # save as csv file
    path_to_shapefile <- "~/shape_files/"
    if (!dir.exists(path_to_shapefile)){
        dir.create(path_to_shapefile)
    }

    file_name <- paste0(path_to_shapefile, state, "_",
                        tolower(geo_header), year, ".csv")
    fwrite(combined, file = file_name)

    return(combined)
}


