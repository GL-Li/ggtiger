# examples
# library(data.table)
# library(magrittr)
# library(maps)
# library(stringr)
# library(tigris)

library(ggshapefile)
library(totalcensus)


# example 1
ri <- get_map("Providence, RI", zoom = 10, color = "bw")

filldata <- read_acs5year(
    year = 2016,
    states = "RI",
    areas = "Providence county, RI",
    table_contents = "white = B02001_002",
    summary_level = "tract"
) %>%
    .[, white_ratio := white / population] %>%
    .[, .(GEOID, white_ratio)]

ggmap(ri) +
    # in order to inherit data and x, y from ggmap, they must NOT be changed
    # in geom_boundary(). Then how to feed aes to polygon
    geom_boundary(geometry = "tract", geom = "polygon", data_fill = filldata,
                  aes(group = ..group.., fill = ..value..),
                  color = "grey", size = 0.1, alpha = 0.8) +
    geom_boundary(geometry = "county subdivision", color = "blue", fill = NA) +
    scale_fill_gradient(na.value = NA, low = "green", high = "red")


# example 2
prov <- get_map("Providence, RI", zoom = 13, color = "bw")
filldata <- read_acs5year(
    year = 2016,
    states = "RI",
    areas = "Providence city, RI",
    table_contents = "white = B02001_002",
    summary_level = "tract"
) %>%
    .[, white_ratio := white / population] %>%
    .[, .(GEOID, white_ratio)]

block_data <- read_decennial(
    year = 2010,
    states = "RI",
    areas = "Providence city, RI",
    summary_level = "block"
)

ggmap(prov) +
    geom_boundary(geometry = "tract", data_fill = filldata,
                  aes(group = ..group.., fill = ..value..),
                  color = "green", size = 0.5, alpha = 1, fill = NA) +
    geom_boundary(geometry = "county subdivision", color = "red",
                  linetype = "dotted", fill = NA) +
    geom_point(data = block_data[population != 0], aes(lon, lat, size = population),
               color = "red", alpha = 0.5) +
    scale_size_area(max_size = 5)
