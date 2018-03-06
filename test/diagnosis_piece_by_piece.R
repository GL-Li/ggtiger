

library(ggtiger)
library(totalcensus)
library(data.table)
library(magrittr)
library(maps)
library(stringr)



# all needed ===============
ri <- get_map("scituate, RI, united states", zoom = 10, color = "bw")
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
    geom_boundary("tract",state = "RI", data_fill = filldata, color = "red") +
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
    geom_boundary("state", state = "RI", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county", state = "RI", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("county subdivision", state = "RI", fill = NA, color = "red")

ggmap(ri) +
    geom_boundary("tract", state = "RI", data_fill = filldata, color = "red") +
    scale_fill_gradient(na.value = NA, low = "green", high = "red")

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
                  data_fill = filldata, alpha = 0.7) +
    geom_boundary("county", color = "green", size = 0.5) +
    geom_boundary("state", color = "red", size = 0.5) +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange") +
    labs(fill = "ratio of\nwhite")

ggmap(ri) +
    geom_boundary("block group", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "red")

