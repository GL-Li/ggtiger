

library(ggshapefile)
library(totalcensus)
library(data.table)
library(magrittr)
library(stringr)
library(maps)
library(tigris)


# example 1: RI ===============
ri <- get_map("scituate, RI, united states", zoom = 10, color = "bw")

filldata <- read_acs5year(
    year = 2016,
    states = "RI",
    areas = "Providence county, RI",
    table_contents = "white = B02001_002",
    summary_level = "tract"
) %>%
    .[, white_ratio := white / population] %>%
    .[, .(GEOID, white_ratio)]


#  paramerters case 1: feed bbox =====
aaa = ggmap(ri)
data = aaa$data
data$x = data$lon
data$y = data$lat
state = NULL
county = NULL
N = 100
geography = "county"
year = 2016
data_fill = NULL

# parameters case 2: feed state and county ====
state = "RI"
county = c("providence", "kent")
year = 2016
geography = "tract"
year = 2016
data_fill = NULL

# parameters case 3: feed state ====
state = "RI"
county = NULL
year = 2016
geography = "tract"
year = 2016
data_fill = NULL




ggmap(ri) +
    geom_boundary(geography = "state", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("place", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary(geography = "county", fill = NA, color = "blue")

ggmap(ri) +
    geom_boundary(geography = "county", state = "RI", fill = NA, color = "blue")


ggmap(ri) +
    geom_boundary(geography = "tract", fill = NA, color = "red")


ggmap(ri) +
    geom_boundary("block group", fill = NA, color = "cyan")



ggmap(ri) +
    geom_boundary("block group", fill = NA, color = "cyan",
                  state = "RI", county = c("providence", "kent"))

ggmap(ri) +
    geom_boundary("tract", state = "RI", fill = NA, color = "green")


ggmap(ri) +
    geom_boundary("county", fill = NA, color = "blue") +
    geom_boundary("state", fill = NA, color = "black") +
    # in order to inherit data and x, y from ggmap, they must NOT be changed
    # in geom_boundary(). Then how to feed aes to polygon
    geom_boundary(geography = "tract", data_fill = filldata,
                  color = "grey", size = 0.1, alpha = 0.8) +
    geom_boundary(geography = "county subdivision", state = "RI", color = "blue", fill = NA) +
    scale_fill_gradient(na.value = NA, low = "green", high = "red")





# example 2: Providence ==============
prov <- get_map("Providence, RI", zoom = 12, color = "bw")
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
    geom_boundary(geography = "tract", data_fill = filldata,
                  state = "RI", county = "Providence",
                  color = "green", size = 0.5, alpha = 0.5) +
    geom_boundary(geography = "county subdivision",
                  state = "RI", county = "Providence", color = "red",
                  linetype = "dotted", fill = NA) +
    geom_point(data = block_data[population != 0], aes(lon, lat, size = population),
               color = "red", alpha = 0.5) +
    scale_size_area(max_size = 5) +
    scale_fill_continuous(na.value = NA)

# example 3: USA =====
us <- get_map("united states", zoom = 8, color = "bw")



ggmap(us) +
    geom_boundary("state", color = "red", fill = NA) +
    geom_boundary("place", color = "green", fill = NA)
