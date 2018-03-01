# library(data.table)
# library(magrittr)
# library(maps)
# library(stringr)
# library(tigris)

get_coord_names <- function(locations){
    # get county and state names of locations
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

    if ("long" %in% names(locations)){
        setnames(locations, "long", "lon")
    }


    name <- map.where("county", locations$lon, locations$lat) %>%
        # return NA if a point is not inside any state
        na.omit() %>%
        unique()


    dt <- data.table(
        state = str_extract(name, "^[^,]*"),
        county = str_extract(name, "[^,]*$"),
        stringsAsFactors = FALSE
    )

    # convert to state abbreviation

    state_names <- data.table(
        abbr = c(state.abb, "DC"),    # no DC in state.abb
        state = tolower(c(state.name, "District of Columbia"))
    )

    ans <- state_names[dt, on = .(state)] %>%
        .[, .(abbr, county)]
    return(ans)
}


spread_coord <- function(bbox, N = 100){
    # spread a rectangle into N^2 points grid
    #
    # Args_____
    # bbox: bounding box of the rectangle as c(left, bottom, right, top) or
    #     c(x_min, y_min, x_max, y_max)
    # N: number of points along each axis
    #
    # Return_____
    # a data frame

    x <- seq(bbox[1], bbox[3], length.out = N)
    y <- seq(bbox[2], bbox[4], length.out = N)
    coords <- expand.grid(x, y)
    names(coords) <- c("lon", "lat")

    return(coords)
}


keep_geoid <- function(dt, bbox){
    # keep GEOID in dt that has any row within the map range
    #
    # Args_____
    # dt: data.table that generated from shape files.
    # bbox: bounding box of the rectangle as c(left, bottom, right, top) or
    #     c(x_min, y_min, x_max, y_max).
    #
    # Return_____
    # data.table with selected GEOIDs from dt


    geoid_keep <- dt[x >= bbox[1] & x <= bbox[3] & y >= bbox[2] & y <= bbox[4],
                     unique(GEOID)]
    dt <- dt[GEOID %in% geoid_keep]
}


fips2name_state <- function(state_fips, type = "abbr"){
    # convert state fips to state names
    #
    # Args_____
    # state_fips: state fips
    # type: type of state names, "state" or "abbr"
    #
    # Return_____
    # vector of state names
    fips_in <- data.table(fips = state_fips)

    all_fips <- state.fips %>%
        setDT() %>%
        .[, state := str_extract(polyname, "^[^:]*")] %>%
        .[, polyname := NULL] %>%
        unique() %>%
        .[, .(fips, abbr = abb, state)] %>%
        # why no HI in the database
        rbind(data.table(
            fips = 15,
            abbr = "HI",
            state = "hawaii"
        )) %>%
        .[, fips := ifelse(fips < 10, paste0("0", fips), fips)]

    state_name <- all_fips[fips_in, on = .(fips)] %>%
        .[, get(type)]
    return(state_name)
}



names2fips_state <- function(state_names, type = "abbr"){
    # convert state names to state fips
    #
    # Args_____
    # state_names: lowercase or abbreviation of state names
    # type: type of state names, "state" or "abbr"
    #
    # Return_____
    # vector of state fips
    if (type == "abbr"){
        names_in <- data.table(abbr = state_names)
    } else if (type == "state"){
        names_in <- data.table(state = state_names)
    }


    all_fips <- state.fips %>%
        setDT() %>%
        .[, state := str_extract(polyname, "^[^:]*")] %>%
        .[, polyname := NULL] %>%
        unique() %>%
        .[, .(fips, abbr = abb, state)] %>%
        # why no HI in the database
        rbind(data.table(
            fips = 15,
            abbr = "HI",
            state = "hawaii"
        )) %>%
        .[, fips := ifelse(fips < 10, paste0("0", fips), fips)]

    if (type == "abbr"){
        state_fips <- all_fips[names_in, on = .(abbr)] %>%
            .[, fips]
    } else if (type == "state") {
        state_fips <- all_fips[names_in, on = .(state)] %>%
            .[, fips]
    }

    return(state_fips)
}



fips2names_county <- function(states, county_fips){
    # convert county fips code in a state to county name
    #
    # Args_____
    # states: lower case state names
    # COUNTY: county fips code within state, such as "011'
    #
    # Return_____
    # vector of county names in lower case

    if (sum(!is.na(county_fips)) == 0){
        return(rep(NA, length(county_fips)))
    }

    fips_in = data.table(fips = county_fips, state = states)

    all_fips <- county.fips %>%
        setDT() %>%
        .[, fips := str_extract(fips, ".{3}$")] %>%
        .[, state := str_extract(polyname, "^[^,]*")] %>%
        .[, county := str_extract(polyname, "[^,]*$")] %>%
        .[, polyname := NULL] %>%
        # county such as "san juan:lopez island" and "san juan:orcas island"
        # has the same key (state, fips), keep only one of them
        .[, county := str_extract(county, "^[^:]*")] %>%
        unique() %>%
        # still one duplicates "067, montana, park" and
        # "067, montana, yellowstone national", keep only the later
        .[!(fips == "067" & state == "montana" & county == "park")]

    county_name <-  all_fips[fips_in, on = .(state, fips)] %>%
        .[, county]
    return(county_name)
}


geography2geoheader <- function(geography){
    # convert geography to geo_header
    geoheader <- switch (geography,
                         state = "STATE",
                         county = "COUNTY",
                         place = "PLACE",
                         "county subdivision" = "COUSUB",
                         metro = "CBSA",
                         tract = "TRACT",
                         "block group" = "BLKGRP",
                         zipcode = "ZCTA5"
    )
    geoheader
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
