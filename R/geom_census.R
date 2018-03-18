# user functions ==============================================================

#' Plot census data on ggmap
#'
#' @param survey "decennial" for decennial census, "acs5year" for ACS 5-year
#'     estimates, "acs1year" for ACS 1-year estimate
#' @param year year of the survey
#' @param table_contents contents of table to be read
#' @param summary_level select which summary level to keep, "*" to keep all. It
#'     takes strings including "state", "county", "county subdivision", "place",
#'     "tract", "block group", and "block" for the most common levels. It also
#'     take code for level.
#' @param states abbreviation of a state of which data to be read, such as "RI".
#' @param areas areas of which data to be read
#' @param N integer when state is NULL, N x N grid points on the view of map are
#'     used to determine the states and counties within the view.
#' @param mapping same as those in ggplot functions, typically inherited from ggmap()
#' @param data data typically inherited from ggmap().
#' @param geom same as those in ggplot2 functions but should be uisng "point".
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


geom_census <- function(survey, year, table_contents, summary_level,
                        states = NULL, areas = NULL, N = 100,
                          mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          ...){
    layer(
        stat = StatCensus, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        # all parameters inside list
        params = list(survey = survey,
                      year = year,
                      table_contents = table_contents,
                      summary_level = summary_level,
                      states = states,
                      areas = areas,
                      N = N,
                      na.rm = na.rm,
                      ...)
    )
}




StatCensus <- ggproto(
    "StatCensus", Stat,
    required_aes = c("x", "y"),
    default_aes = aes(size = ..population..),
    compute_group = function(data, scales,
                             params, survey, year, table_contents, summary_level,
                             states = NULL, areas = NULL, N){

        bbox <- c(min(data$x), min(data$y), max(data$x), max(data$y))

        # find out all state-county pairs
        if (is.null(states)){
            grid_points <- spread_coord(bbox, N)
            state_county <- get_coord_names(grid_points)
            states <- state_county[, unique(abbr)]

            # maps::map.where does work in AK and HI, use all counties
            if (nrow(state_county) == 0){
                if (bbox[2] > 40) states <- "AK"
                if (bbox[2] < 40) states <- "HI"
            }
        }



        # read census data
        if (survey %in% c("decennial", "dec")){
            dt <- read_decennial(
                year = year,
                states = states,
                table_contents = table_contents,
                areas = areas,
                summary_level = summary_level
            )
        } else if (survey %in% c("acs5year", "acs5")){
            dt <- read_acs5year(
                year = year,
                states = states,
                table_contents = table_contents,
                areas = areas,
                summary_level = summary_level
            )
        } else if (survery %in% c("acs1year", "acs1")){
            dt <- read_acs1year(
                year = year,
                states = states,
                table_contents = table_contents,
                areas = areas,
                summary_level = summary_level
            )
        }

        # select data within map view
        setnames(dt, c("lon", "lat"), c("x", "y"))

        dt <- dt[population != 0] %>%
            .[x >= bbox[1] & x <= bbox[3] & y >= bbox[2] & y <= bbox[4]]

        return(dt)
    }
)


#' Draw census boudaries on ggmap
#'
#' The same as geom_boundary()
#'
#' @param ... see all parameters in geom_boundary
#'

stat_census <- function(...){
    geom_census(... = )
}

