library(ggtiger)
library(totalcensus)
library(data.table)
library(magrittr)


# Median home value in Providence county at block group level ===============
ri <- get_map("scituate, RI, united states", zoom = 10, color = "bw")

home_value <- read_acs5year(
    year = 2016,
    states = "RI",
    table_contents = "home_value = B25077_001",
    areas = c("Providence county, RI",
              "washington county, RI"),
    summary_level = "tract"
)

value_fill <- home_value[, .(GEOID, home_value = as.numeric(home_value))]

# fill tract
ggmap(ri) +
    geom_boundary("tract", state = "RI", county = c("providence", "washington"),
                  data_fill = value_fill, alpha = 0.9) +
    geom_boundary("county", color = "green", size = 0.5) +
    geom_boundary("state", color = "red", size = 0.5) +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange") +
    labs(fill = "home_value")


# point + boundary
ggmap(ri) +
    geom_point(data = home_value, aes(lon, lat, size = home_value),
               color = "red", alpha = 0.5) +
    geom_boundary("tract", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "orange", size = 0.2) +
    geom_boundary("county", fill = NA, color = "green", size = 0.5) +
    geom_boundary("state", fill = NA, color = "blue", size = 0.5) +
    scale_size_area(max_size = 4) +
    labs(fill = "ratio of\nwhite")
