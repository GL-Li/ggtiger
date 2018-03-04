library(totalcensus)
library(ggtiger)
library(data.table)
library(magrittr)


# test get_coord_names function ===============================================
test <- read_acs1year(2016, "US", summary_level = "310") %>%
    # remove NAs from coordinate
    .[!(is.na(lon) | is.na(lat))]

gcn_1 <- ggtiger:::get_coord_names(test)
stopifnot(dim(gcn_1) == c(479, 2))



# test spread_coords ===========================================================
bbox <- c(-100, 30, -80, 45)
df <- ggtiger:::spread_coord(bbox)
sc1 <- ggtiger:::get_coord_names(df)
stopifnot(dim(sc1) == c(1893, 2))

message("Passed test.")
