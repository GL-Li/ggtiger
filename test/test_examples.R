library(ggtiger)
library(totalcensus)
library(data.table)
library(magrittr)


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



ggmap(ri) +
    geom_boundary("block group", fill = NA, color = "cyan",
                  state = "RI", county = "Providence") +
    geom_boundary("tract", state = "RI", fill = NA, color = "green") +
    geom_boundary("county", fill = NA, color = "blue") +
    geom_boundary("state", fill = NA, color = "black")

ggmap(ri) +
    geom_boundary("tract", data_fill = filldata, color = "red",
                  size = 0.1, alpha = 0.8) +
    geom_boundary("county subdivision", state = "RI", color = "blue", fill = NA) +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange")


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
us <- get_map("united states", zoom = 7, color = "bw")



ggmap(us) +
    geom_boundary("county", color = "green", fill = NA) +
    geom_boundary("state", color = "red", fill = NA)


# DC, HI, and AK area =====================================
dc <- get_map("district of columbia, USA", zoom = 12, color = "bw")
ggmap(dc) +
    geom_boundary("tract", fill = NA, color = "blue") +
    geom_boundary("state", fill = NA, color = "red")


hi <- get_map("hawaii, USA", zoom = 10, color = "bw")
ggmap(hi) +
    geom_boundary("block group", fill = NA, color = "blue") +
    geom_boundary("state", state = "HI", fill = NA, color = "red")


ak <- get_map("alaska, USA", zoom = 5, color = "bw")
ggmap(ak) +
    geom_boundary("tract", state = "AK",
                  county = c("Anchorage", "Matanuska-Susitna", "southeast fairbanks"),
                  fill = NA, color = "blue", size = 0.2) +
    geom_boundary("county", fill = NA, color = "green", size = 0.5) +
    geom_boundary("state", state = "AK", fill = NA, color = "red")

tx <- get_map("texas, USA", zoom = 6, color = "bw")
ggmap(tx) +
    geom_boundary("tract", state = "TX", fill = NA, color = "blue") +
    geom_boundary("state", fill = NA, color = "red")

