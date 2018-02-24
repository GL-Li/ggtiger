library(totalcensus)
library(ggshapefile)
library(data.table)
library(magrittr)
library(maps)
library(stringr)


# test get_coord_names function ===============================================
test <- read_acs1year(2016, "US", summary_level = "310") %>%
    # remove NAs from coordinate
    .[!(is.na(lon) | is.na(lat))]

gcn_1 <- ggshapefile:::get_coord_names(test)
stopifnot(gcn_1 == c("WA", "TX", "MI", "OH", "NM", "GA", "OR", "NY", "AL", "LA",
                   "PA", "IA", "WI", "NC", "NJ", "ME", "CA", "MD", "WV", "MT",
                   "ND", "VA", "IN", "ID", "MA", "CO", "KY", "MN", "MO", "CT",
                   "VT", "FL", "IL", "NV", "WY", "SC", "TN", "NH", "DE", "OK",
                   "AR", "AZ", "NE", "MS", "KS", "UT", "RI", "SD"))


gcn_2 <- ggshapefile:::get_coord_names(test, "county")
stopifnot(gcn_2[1:10] == c("grays harbor", "taylor", "lenawee", "portage",
                           "otero", "dougherty", "linn", "schenectady",
                           "marshall", "bernalillo"))

# test spread_coords ===========================================================
xlim <- c(-100, -80)
ylim <- c(30, 45)
df <- ggshapefile:::spread_coord(xlim, ylim)
states <- ggshapefile:::get_coord_names(df)
stopifnot(states == c("TX", "LA", "FL", "MS", "AL", "GA", "SC", "AR", "OK", "TN",
                      "NC", "MO", "KY", "VA", "KS", "IL", "WV", "IN", "OH", "PA", "NE",
                      "IA", "MI", "SD", "WI", "MN"))
