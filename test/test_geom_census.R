library(ggtiger)
library(viridis)
cambridge <- get_map("cambridge, MA, usa", zoom = 12, color = "bw")


aaa = ggmap(cambridge)
data = aaa$data
data$x = data$lon
data$y = data$lat

survey = "decennial"
states = NULL
table_contents = "white = P0030002"
summary_level = "block"
area = NULL
N = 100
year = 2010


ggmap(cambridge) +
    geom_census("decennial", 2010, summary_level = "block",
                table_contents = "white = P0030002",
                mapping = aes(color = ..white../..population..),
                alpha = 0.5) +
    scale_color_viridis() +
    scale_size_area(max_size = 4, breaks = c(10, 100, 500, 2000)) +
    geom_boundary("county subdivision", fill = NA, color = "red")



# RI =========================================
ri <- get_map("providence, RI, united states", zoom = 12, color = "bw")

# white people
ggmap(ri) +
    geom_census("decennial", 2010, summary_level = "block",
                table_contents = "white = P0030002", states = "RI",
                mapping = aes(color = ..white../..population..),
                alpha = 0.7) +
    scale_color_viridis() +
    scale_size_area(max_size = 4, breaks = c(10, 100, 500, 2000)) +
    geom_boundary("county subdivision", states = "RI", fill = NA, color = "red") +
    labs(color = "white ratio")


# black people
ggmap(ri) +
    geom_census("decennial", 2010, summary_level = "block",
                table_contents = "black = P0030003", states = "RI",
                mapping = aes(color = ..black../..population..),
                alpha = 0.7) +
    scale_color_viridis() +
    scale_size_area(max_size = 4, breaks = c(10, 100, 500, 2000)) +
    geom_boundary("county subdivision", states = "RI", fill = NA, color = "red") +
    labs(color = "black ratio")

sf <- get_map("san francisco, usa", zoom = 10, color = "bw")
ggmap(sf) +
    geom_census("acs5", 2016, summary_level = "block group",
                table_contents = "asian = C02003_006",
                mapping = aes(color = ..asian../..population..),
                alpha = 0.5) +
    scale_color_viridis() +
    scale_size_area(max_size = 2, breaks = c(10, 100, 500, 2000)) +
    geom_boundary("county subdivision", fill = NA, color = "red", size = 0.3) +
    labs(color = "asian ratio")
