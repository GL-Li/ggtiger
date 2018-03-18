
library(ggtiger)
library(viridis)

# load saved ggmaps
# load("meetup_Boston_data_vis_20180319/saved_maps.RData")

# gerrymandering in PA ========================================================

# download map near philly in PA
philly <- get_map("pottstown, PA,  usa", zoom = 9, color = "bw")

### congressional district boudaries
ggmap(philly) +
    geom_boundary("congressional district", states = "PA",
                  mapping = aes(fill = ..GEOID..),
                  alpha = 0.7, color = "red", size = 0.3) +
    scale_fill_brewer(palette = "PuOr") +
    labs(title = "Congressional district gerrymandering in PA")


### percentage of white people in each block
ggmap(philly) +
    # plot demographic at each census block as a point, sized by population
    # and colored by ratio of white people
    geom_census("decennial", year = 2010, states = "PA",
                table_contents = "white = P0030002",
                summary_level = "block",
                mapping = aes(color = 100 * ..white../..population..)) +
    # draw congressional district boundaries
    geom_boundary("congressional district", states = "PA",
                  fill = NA, color = "red", size = 0.2) +
    scale_size_area(max_size = 1) +
    scale_color_viridis() +
    guides(size = "none") +
    labs(title = "Percentage of white people in each census block",
         color = "white\npercent")


### rent near Cambridge =======================================================

# download map
cambridge <- get_map("cambridge, MA, usa",
                     zoom = 13, color = "bw")

# plot
ggmap(cambridge) +
    geom_boundary("block group",  states = "MA", fill = NA,
                  color = "orange", size = 0.3) +
    geom_boundary("county subdivision", fill = NA, color = "blue", size = 0.5) +
    geom_census("acs5year", 2016, summary_level = "block group",
                table_contents = "rent = B25064_001",
                mapping = aes(color = ..rent..), alpha = 0.8) +
    scale_color_viridis(option = "B") +
    scale_size_area(max_size = 6) +
    labs(title = "Monthly rent near Cambridge in each block group")
