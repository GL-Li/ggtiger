#' Add census boudaries to ggmap
#'
#' @export


geom_boundary <- function(mapping = NULL, data = NULL, geom = "polygon",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, geometry, data_fill = NULL, year = 2016,
                     ...){
    layer(
        stat = StatBoundary, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        # all parameters inside list
        params = list(geometry = geometry,
                      data_fill = data_fill,
                      year = year,
                      na.rm = na.rm,
                      ...)
    )
}




StatBoundary <- ggproto(
    "StatBoundary", Stat,
    required_aes = c("x", "y"),
    # default_aes = aes(x = ..long..,
    #                   y = ..lat..,
    #                   group = ..group..,
    #                   fill = ..value..),
    compute_group = function(data, scales,
                             params, geometry, data_fill, year){
        print(data)
        xlim <- c(min(data$x), max(data$x))
        ylim <- c(min(data$y), max(data$y))
        grid_points <- spread_coord(xlim, ylim)
        states <- get_coord_names(grid_points)

        geoheader <- switch (geometry,
            state = "STATE",
            county = "COUNTY",
            place = "PLACE",
            "county subdivision" = "COUSUB",
            metro = "CBSA",
            tract = "TRACT",
            "block group" = "BLKGRP"
        )

        dt <- purrr::map(states, download_shapefile,
                         geo_header = geoheader, year) %>%
            rbindlist() %>%
            # rename for inherited aes
            setnames(c("long", "lat"), c("x", "y"))


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

