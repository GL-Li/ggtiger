

library(ggtiger)
library(totalcensus)
library(data.table)
library(magrittr)
library(maps)
library(stringr)

cambridge <- get_map("cambridge, MA, usa", zoom = 13, color = "bw")
aaa <- ggmap(cambridge)
# all needed ===============
ri <- get_map("new port, RI, united states", zoom = 9, color = "bw")
aaa = ggmap(ri)
data = aaa$data
data$x = data$lon
data$y = data$lat


filldata <- read_acs5year(
    year = 2016,
    states = "RI",
    areas = "Providence county, RI",
    table_contents = "white = B02001_002",
    summary_level = "tract"
) %>%
    .[, white_ratio := white / population] %>%
    .[, .(GEOID, white_ratio)]


#  by bbox =====
state = NULL
county = NULL
N = 100
geography = "block group"
year = 2016
data_fill = NULL

ggmap(ri) +
    geom_boundary("state", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county subdivision", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("tract", data_fill = filldata, color = "red",
                  mapping = aes(fill = ..white_ratio..)) +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange")

ggmap(ri) +
    geom_boundary("block group", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("zip code", fill = "green", alpha = 0.3, color = "red") +
    geom_boundary("county", fill = NA, color = "green", linetype = "dashed", size = 0.5)





# state only ====
state = "RI"
county = NULL
year = 2016
geography = "tract"
year = 2016
data_fill = NULL

ggmap(ri) +
    geom_boundary("state", state = c("RI", "NY"), fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county", state = c("RI", "CT"), fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county subdivision", state = c("RI", "CT"), fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("tract", state = "RI", data_fill = filldata, color = "red",
                  mapping = aes(fill = ..white_ratio..)) +
    scale_fill_gradient(na.value = NA, low = "orange", high = "cyan")

ggmap(ri) +
    geom_boundary("block group", state = "RI", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("zip code", state = "RI", fill = "green", alpha = 0.3, color = "red")



# state and county ====
state = "RI"
county = c("providence", "washington")
year = 2016
geography = "tract"
year = 2016
data_fill = NULL

ggmap(ri) +
    geom_boundary("zip code", state = "RI", county = c("providence", "washington"),
                  fill = "green", alpha = 0.3, color = "red")


ggmap(ri) +
    geom_boundary("county subdivision", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "red")


ggmap(ri) +
    geom_boundary("tract", state = "RI", county = c("providence", "washington"),
                  data_fill = filldata, alpha = 0.7,
                  mapping = aes(fill = ..white_ratio..)) +
    geom_boundary("county", color = "green", size = 0.5, fill = NA) +
    geom_boundary("state", color = "red", size = 0.5, fill = NA) +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange") +
    labs(fill = "ratio of\nwhite")

ggmap(ri) +
    geom_boundary("block group", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "red")


# congressional district ======================================================
race <- read_decennial(
    year = 2010,
    states = "PA",
    table_contents = "white = P0030002",
    summary_level = "block"
) %>%
    .[population != 0] %>%
    .[, white_pct := white / population] %>%
    .[order(-white_pct)]

philly <- get_map("pottstown, PA,  usa", zoom = 9, color = "bw")
philly_zoom <- get_map("pheladlphia,PA, USA", zoom = 10, color = "bw")

ggmap(philly_zoom) +
    geom_point(data = race, aes(lon, lat, size = population,
                                color = white_pct), alpha = 0.3) +
    geom_boundary("congressional district", state = "PA",
                  #mapping = aes(fill = ..GEOID..),
                  alpha = 0.2, color = "blue", size = 0.2) +
    scale_fill_brewer(palette = "PuOr") +
    scale_size_area(max_size = 1) +
    scale_color_gradient(low = "green", high = "red")

ggmap(philly) +
    geom_boundary("congressional district", state = "PA",
                  mapping = aes(fill = ..GEOID..),
                  alpha = 0.7, color = "red", size = 0.3) +
    scale_fill_brewer(palette = "PuOr")


ggmap(philly) +
    geom_boundary("county", state = "PA", color = "green", fill = NA) +
    scale_fill_brewer(palette = "PuOr")

ggmap(philly) +
    geom_boundary("congressional district", state = "PA",
                  mapping = aes(fill = ..GEOID..),
                  alpha = 0.7, color = "red", size = 0.3) +
    geom_point(data = race, aes(lon, lat, size = population,
                                color = black_pct), alpha = 0.3) +
    scale_fill_brewer(palette = "PuOr") +
    scale_size_area(max_size = 2) +
    labs(title = "Congressional districts in Pennsylvania near Philadelphia")


ggmap(philly) +
    geom_point(data = race[lon > -76.51 & lon < -74.5 & lat > 39.5 & lat < 41],
               aes(lon, lat, size = population, color = white_pct),
               alpha = 0.2) +
    geom_boundary("congressional district", state = "PA",
                  #mapping = aes(fill = ..GEOID..), alpha = 0.4,
                  color = "blue", size = 0.5, fill = NA) +
    scale_fill_brewer(palette = "PuOr") +
    scale_size_area(max_size = 1) +
    scale_color_gradient(low = "green", high = "red") +
    guides(size = "none")
