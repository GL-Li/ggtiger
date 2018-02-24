# library(data.table)
# library(magrittr)
# library(maps)
# library(stringr)
# library(tigris)

get_coord_names <- function(locations, geography = "state"){
    # get names of locations
    #
    # Args_____
    # locations: a data frame contains column of "lon" (or "long") and "lat"
    # geography: The geography of which names to return. "state" to get state
    #     names, "county" to get county names.
    #
    # Return_____
    # vector of names

    # use data.table
    setDT(locations)

    if (geography == "state") database = "state"
    if (geography == "county") database = "county"


    if ("long" %in% names(locations)){
        setnames(locations, "long", "lon")
    }

    NAME <- map.where(database = database, locations$lon, locations$lat) %>%
        # return NA if a point is not inside any state
        na.omit() %>%
        # some return has :main after state names, get rid of :main
        str_extract("^[^:]*") %>%
        unique()

    # get state abbreviation
    if (geography == "state"){
        state_names <- c(state.abb, "DC")
        names(state_names) <- tolower(c(state.name, "District of Columbia"))
        NAME <- unname(state_names[NAME])
    }

    # get county name without state name
    if (geography == "county"){
        NAME <- str_extract(NAME, "[^,]*$")
    }

    return(NAME)
}


spread_coord <- function(xlim, ylim, N = 50){
    # spread a rectangle into N^2 points grid
    #
    # Args_____
    # xlim: c(min_x, max_x), range of x
    # ylim: c(min_y, max_y), range of y
    # N: number of points along each axis
    #
    # Return_____
    # a data frame

    x <- seq(xlim[1], xlim[2], length.out = N)
    y <- seq(ylim[1], ylim[2], length.out = N)
    coords <- expand.grid(x, y)
    names(coords) <- c("lon", "lat")

    return(coords)
}

#
# plot_shapefile <- function(geo_header, year, lon_1, lat_1, lon_2, lat_2){
#     # plot shapefile in rectangle range
#     #
#     # geo_header: geography to be plotted
#     # year of the shape file
#     # lon_1, lat_1: coordinate of point at the bottom left corner
#     # lon_2, lat_2: coordinate of point at the top right corner
#     #
#     grid_points <- spread_coord(c(lon_1, lon_2), c(lat_1, lat_2))
#     states <- get_coord_names(grid_points)
#     dt <- purrr::map(states, download_shapefile,
#                      geo_header = geo_header, year = year) %>%
#         rbindlist()
#     ggmap(ri) +
#         geom_map(data = dt, map = dt, aes(long, lat, map_id = id, fill = NAME),
#                  color = "red", alpha = 0.1, size=0.1, show.legend = FALSE)
# }


# # tryout
# library(ggmap)
# ri <- get_map("Rhode Island", zoom = 11, color = "bw")
# aaa <- ggmap(ri)
# aaa$data
#     #         lon      lat
#     # 1 -71.69681 41.41527
#     # 2 -71.25736 41.41527
#     # 3 -71.69681 41.74399
#     # 4 -71.25736 41.74399
#
# lon_1 <- min(aaa$data["lon"])
# lon_2 <- max(aaa$data["lon"])
# lat_1 <- min(aaa$data["lat"])
# lat_2 <- max(aaa$data["lat"])
#
# grid_points <- spread_coord(c(lon_1, lon_2), c(lat_1, lat_2))
# states <- get_coord_names(grid_points)
# dt <- purrr::map(states, download_shapefile,
#                  geo_header = "COUSUB", year = 2016) %>%
#     rbindlist()
# ggmap(ri) +
#     geom_map(data = dt, map = dt, aes(long, lat, map_id = id, fill = id),
#              fill = NA, color = "red", alpha = 0.2, size=0.1, show.legend = FALSE) +
#     geom_text(data = unique(dt[, .(NAME, INTPTLON, INTPTLAT)]),
#               aes(INTPTLON, INTPTLAT, label = NAME))
#
#
#
#
#
# # geom_boundary <- function(geo_header){
# #     lon_1 <- -71.7 #min(data["lon"])
# #     lon_2 <- -71.3 #max(data["lon"])
# #     lat_1 <- 41.41 #min(data["lat"])
# #     lat_2 <- 41.74 #max(data["lat"])
# #
# #     grid_points <- spread_coord(c(lon_1, lon_2), c(lat_1, lat_2))
#     states <- get_coord_names(grid_points)
#     dt <- purrr::map(states, download_shapefile,
#                      geo_header = "COUSUB", year = 2016) %>%
#         rbindlist()
#     geom_map(data = dt, map = dt, aes(long, lat, map_id = id, fill = id),
#              fill = NA, color = "red", alpha = 0.2, size=0.1, show.legend = FALSE)
#     # layer(data = dt, mapping = mapping, stat = stat, geom = GeomMap,
#     #       position = PositionIdentity, show.legend = show.legend,
#     #       inherit.aes = inherit.aes, params = list(map = map, na.rm = na.rm, ...))
# }
#
# ggmap(ri) +
#     geom_boundary(geo_header = "COUSUB")
#
#
#
# bbb <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
#     geom_point() +
#     geom_line()
# bbb
# bbb$layers
# names(bbb)
# bbb$data
# bbb$mapping
