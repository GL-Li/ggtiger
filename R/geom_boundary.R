#' Add census boudaries to ggmap
#'
#' @export


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
                      fill = ..value..),
    compute_group = function(data, scales,
                             params, geography, data_fill, year, state, county, N){

        path_to_shapefile <- "~/shape_files"

        print(data)
        bbox <- c(min(data$x), min(data$y), max(data$x), max(data$y))
        xlim <- c(bbox[1], bbox[3])
        ylim <- c(bbox[2], bbox[4])



        if (geography %in% c("state")){
            # state boundary has one national file for all states
            file_name <- paste0(path_to_shapefile, "/",
                                tolower(str_replace(geography, " ", "_")),
                                "_all_states_", year, ".csv")
            if (file.exists(file_name)){
                dt <- get_existing(file_name, bbox)
            } else {
                dt <- download_shapefile(geography = geography, year = year) %>%
                    # rename for inherited aes
                    setnames(c("long", "lat"), c("x", "y")) %>%
                    keep_geoid(bbox)
            }

        }




#
#         if (geography %in% c("county")){
#             # county boundary has one national file for all states and counties
#         }





        if (geography == "zip code"){
            # need special treatment
            NULL
        }


        if (geography %in% c("county", "county subdivision", "place", "tract",
                             "block group")) {
            # find out all state-county pairs
            if (is.null(state)){
                grid_points <- spread_coord(bbox, N)
                state_county <- get_coord_names(grid_points)
            } else {
                state_county <- data.table(
                    abbr = state,
                    county = ifelse(is.null(county), "all_counties", county)
                )
            }

            # process state by state
            states <- state_county[, unique(abbr)]

            dt_list <- list()
            for (st in states){
                if (geography == "county"){
                    file_name <- paste0(
                        path_to_shapefile, "/",
                        tolower(str_replace(geography, " ", "_")),
                        "_all_states_", year, "_all_counties.csv"
                    )
                } else {
                    file_name <- paste0(path_to_shapefile, "/",
                                        tolower(str_replace(geography, " ", "_")),
                                        "_", st, "_", year, ".csv")
                }

                if (file.exists(file_name)){
                    counties <- state_county[abbr ==st, county]
                    file_names <- paste0(
                        path_to_shapefile, "/",
                        tolower(str_replace(geography, " ", "_")),
                        "_", st, "_", year, "_", counties, ".csv"
                    )
                    dt <- lapply(file_names, get_existing, bbox) %>%
                        rbindlist()
                } else {
                    if (geography == "county"){
                        dt <- download_shapefile(geography = "county", year = year) %>%
                            # rename for inherited aes
                            setnames(c("long", "lat"), c("x", "y")) %>%
                            keep_geoid(bbox)
                    } else {
                        dt <- download_shapefile(state = st,
                                                 geography = geography,
                                                 year = year) %>%
                            # rename for inherited aes
                            setnames(c("long", "lat"), c("x", "y")) %>%
                            keep_geoid(bbox)
                    }

                }

                dt_list <- c(dt_list, list(dt))

                # assign stay in the local enviroment of for loop
                # assign(paste0("dt_", st), dt)
                # message(paste0("dt_", st))
                # print(get(paste0("dt_", st)))
            }

            dt <- rbindlist(dt_list)
        }




        if (is.null(data_fill)){
            # NA is is logic by default, have to be numeric
            dt[, value := as.numeric(NA)]
        } else {
            stopifnot(is.data.frame(data_fill))
            if (!names(data_fill)[1] == "GEOID"){
                stop(paste('The first column of data_fill must be "GEOID" and',
                           "the second column is the numbers to fill."))
            }
            data_fill$value <- data_fill[, 2]

            dt <- setDT(data_fill) %>%
                .[dt, on = .(GEOID)]
        }

        # To plot polygon using ggshape package, group has to be a factor
        # It is specific to polygon. Path do not need it. normal geom_polygon()
        # does not require factor either.
        dt[, group := factor(group)]
        # print(head(dt))
        # print(str(dt))
        # bbb <<- dt

        dt
    }
)



# help functions ==============================================================
get_existing <- function(file_name, bbox){
    dt <- fread(file_name, colClasses = "character") %>%
        .[, x := as.numeric(long)] %>%
        .[, y := as.numeric(lat)] %>%
        keep_geoid(bbox)
}

