# user functions ==============================================================

#' Draw census boudaries on ggmap
#'
#' @param geography geography of which boudaries to be drawn. Currently take
#'     "state", "county", "county subdivision", "tract", "block group".
#' @param data_fill data frame to fill in the boundaries. It has GEOID as the
#'     first column and the values to fill as the second column.
#' @param year year of the shape files updated.
#' @param state abbreviation of a state within which boudaries are drawn, such as "RI".
#' @param county vector of county names of the state, such as c("providence", "kent")
#' @param N integer when state is NULL, N x N grid points on the view of map are
#'     used to determine the states and counties within the view.
#' @param mapping same as those in ggplot functions, typically inherited from ggmap()
#' @param data data typically inherited from ggmap().
#' @param geom same as those in ggplot2 functions but should be uisng "polygon".
#' @param position same as those in ggplot2 functions.
#' @param na.rm same as those in ggplot2 functions.
#' @param show.legend same as those in ggplot2 functions.
#' @param inherit.aes same as those in ggplot2 functions.
#' @param ... same as those in ggplot2 functions.
#'
#' @return a ggplot2 object
#'
#' @export
#'


geom_boundary <- function(geography, data_fill = NULL, year = 2016,
                          state = NULL, county = NULL, N = 100,
                          mapping = NULL, data = NULL, geom = "polygon",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          ...){
    layer(
        stat = StatBoundary, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        # all parameters inside list
        params = list(geography = geography,
                      data_fill = data_fill,
                      year = year,
                      state = state,
                      county = county,
                      N = N,
                      na.rm = na.rm,
                      ...)
    )
}




StatBoundary <- ggproto(
    "StatBoundary", Stat,
    required_aes = c("x", "y"),
    default_aes = aes(group = ..group..,
                      fill = ..fill..),
    compute_group = function(data, scales,
                             params, geography, data_fill, year, state, county, N){

        if (Sys.getenv("PATH_TO_TIGER") == ""){
            message(paste0(
                "\nYou can choose to save processed data to your ",
                'computer for future R sessions.\n',
                'Please check with function "set_path_to_tiger()" for details.'
            ))
            Sys.setenv(PATH_TO_TIGER = tempdir())
        }
        path_to_tiger <- Sys.getenv("PATH_TO_TIGER")


        if (!is.null(county)) county <- tolower(county)

        bbox <- c(min(data$x), min(data$y), max(data$x), max(data$y))
        xlim <- c(bbox[1], bbox[3])
        ylim <- c(bbox[2], bbox[4])

        # find out all state-county pairs
        if (is.null(state)){
            grid_points <- spread_coord(bbox, N)
            state_county <- get_coord_names(grid_points)
        } else if (is.null(county)){
            state_county <- data.table(
                abbr = state,
                county = "all_counties"
            )
        } else if (!is.null(county)){
            state_county <- data.table(
                abbr = state,
                county = county
            )
        }

        # maps::map.where does work in AK and HI, use all counties
        if (nrow(state_county) == 0){
            state_county <- data.table(
                abbr = c("HI", "AK"),
                county = "all_counties"
            )
        }


        # state ---------------------------------------------------------------

        if (geography == "state"){
            states <- state_county[, unique(abbr)]
            file_names <- paste0(path_to_tiger, "/state/",
                                "state_", states, "_", year, ".csv")
            if (any(file.exists(file_names))){
                dt <- get_existing(file_names, bbox)
            } else {
                dt <- download_shapefile(geography = "state", year = year) %>%
                    .[state %in% states]
            }
        }


        # county----------------------------------------------------------------

        if (geography == "county"){
            states <- state_county[, unique(abbr)]
            file_names <- paste0(path_to_tiger, "/county/",
                                 "county_", states, "_", year, "_all_counties.csv")
            if (any(file.exists(file_names))){
                dt <- get_existing(file_names, bbox)
            } else {
                dt <- download_shapefile(geography = "county", year = year) %>%
                    keep_geoid(bbox)
            }

            if (!is.null(county)){
                dt <- dt[state_county, on = .(state = abbr, county)]
            } else if (!is.null(state)){
                dt <- dt[state_county, on = .(state = abbr)]
            }

        }




        # zip code -------------------------------------------------------------

        if (geography == "zip code"){
            if (!is.null(state) & is.null(county)){
                starts_with <- state_zipstart[abbr == state, ZCTA5]
            } else {
                starts_with <- state_county_zipstart[state_county, on = .(abbr, county)] %>%
                    .[, unique(ZCTA5)]
            }

            not_exist <- vector("character")
            exist_file <- vector("character")
            for (st in starts_with){
                # only download whole file one time
                file_name <- paste0(path_to_tiger, "/zcta/startwith_", st, "_", year, ".csv")
                if (file.exists(file_name)){
                    exist_file <- c(exist_file, file_name)
                } else {
                    not_exist <- c(not_exist, st)
                }
            }

            if (length(exist_file) > 0){
                dt_exist <- lapply(
                    exist_file, fread, colClass = c("character", "numeric",
                                                    "numeric", "character")
                ) %>%
                    rbindlist()
            } else {
                dt_exist <- NULL  # for rbindlist()
            }
            if (length(not_exist) > 0){
                dt_not_exist <- download_zipcode(starts_with = not_exist, year = year)
            } else {
                dt_not_exist <- NULL
            }

            dt <- rbindlist(list(dt_exist, dt_not_exist)) %>%
                keep_geoid(bbox)


            # further filter by state and county
            if (!is.null(state) & !is.null(county)){
                zipcode <- all_zipcode[state_county, on = .(abbr, county)] %>%
                    .[, .(ZCTA5 = unique(ZCTA5))]
                dt <- dt[zipcode, on = .(GEOID = ZCTA5)]
            }
            if (!is.null(state) & is.null(county)) {
                zipcode <- all_zipcode[state_county, on = .(abbr)] %>%
                    .[, .(ZCTA5 = unique(ZCTA5))]
                dt <- dt[zipcode, on = .(GEOID = ZCTA5)]
            }

        }






        # others  --------------------------------------------------------------

        # include country subdivision, tract, and block group
        if (geography %in% c("county subdivision", "tract",
                             "block group")) {


            states <- state_county[, unique(abbr)]

            # process state by state
            dt_list <- list()
            for (st in states){
                counties <- state_county[abbr == st, unique(county)]
                geo = str_replace(geography, " ", "_")
                file_names <- paste0(
                    path_to_tiger, "/", geo, "/",
                    geo, "_", st, "_", year, "_", counties,".csv"
                )
                if (any(file.exists(file_names))){
                    dt <- get_existing(file_names, bbox)
                } else {
                    dt <- download_shapefile(state = st, geography = geography, year = year) %>%
                        keep_geoid(bbox)
                    if (sum(counties == "all_counties") == 0){
                        dt <- dt[county %in% counties]
                    }
                }
                dt_list <- c(dt_list, list(dt))
            }

            dt <- rbindlist(dt_list)

        }

        # tempdir(): ubuntu has "/tmp/", windows has "\\Temp", mac has ???, too
        # compilcated.
        # if (grepl("/tmp/|\\Temp|/temp/|/TEMP/", Sys.getenv("PATH_TO_TIGER"))){
        #     message(paste0(
        #         "You can choose to save processed data to your ",
        #         'computer for future R sessions.\n',
        #         'Please check with function "set_path_to_tiger()" for details.'
        #     ))
        # }


        # merge with data_fill -------------------------------------------------

        if (is.null(data_fill)){
            # NA is is logic by default, have to be numeric
            dt[, fill := as.numeric(NA)]
        } else {
            stopifnot(is.data.frame(data_fill))
            if (!names(data_fill)[1] == "GEOID"){
                stop(paste('The first column of data_fill must be "GEOID" and',
                           "the second column is the numbers to fill."))
            }

            # tible gives headaches, turn to data.table
            setDT(data_fill)
            data_fill[, fill := data_fill[, 2]]

            # two types of GEOID, long as 14000US44009051502 and
            # short as 44009051502. Match dt and data_fill' GEOID
            if (all(grepl("US", data_fill$GEOID))){
                data_fill[, GEOID := str_extract(GEOID, "[^(US)]*$")]
            }

            dt <- setDT(data_fill) %>%
                .[dt, on = .(GEOID)]
        }

        # To plot polygon using ggtiger package, group has to be a factor
        # It is specific to polygon. Path do not need it. normal geom_polygon()
        # does not require factor either.
        dt[, group := factor(group)]
        # print(head(dt))
        # print(str(dt))
        # bbb <<- dt

        dt
    }
)


#' Draw census boudaries on ggmap
#'
#' The same as geom_boundary()
#'
#' @param ... see all parameters in geom_boundary
#'

stat_boundary <- function(...){
    geom_boundary(... = )
}


# internal functions ==============================================================
get_existing <- function(file_names, bbox){
    dt <- lapply(file_names, fread, colClasses = "character") %>%
        rbindlist() %>%
        .[, x := as.numeric(x)] %>%
        .[, y := as.numeric(y)] %>%
        keep_geoid(bbox)
}


