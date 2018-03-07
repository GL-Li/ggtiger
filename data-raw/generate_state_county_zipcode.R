library(totalcensus)
library(data.table)
library(magrittr)
zipcode <- read_decennial(
    year = 2010,
    state = states_DC,
    geo_headers = c("STATE", "COUNTY", "ZCTA5"),
    summary_level = "block"
) %>%
    # get rid of 99999, which is for un-decided code
    .[ZCTA5 != "99999"]


# zip start -------------------------------------------------------------------

st_ct_zip <- zipcode[, .(state = ggtiger:::fips2names_state(STATE, "state"),
                         abbr = ggtiger:::fips2names_state(STATE, "abbr"),
                         county = COUNTY, ZCTA5)] %>%
    .[, county := ggtiger:::fips2names_county(state, county)] %>%
    .[, start := stringr::str_extract(ZCTA5, "^.{2}")] %>%
    .[, state := NULL]


state_county_zipstart <- st_ct_zip[, .(ZCTA5 = unique(start)),
                              by = .(abbr, county)]

state_zipstart <- st_ct_zip[, .(ZCTA5 = unique(start)), by = abbr]

save(state_county_zipstart, file = "data/state_county_zipstart.RData")
save(state_zipstart, file = "data/state_zipstart.RData")


# all zip code -----------------------------------------------------------------
all_zipcode <- zipcode[, .(state = ggtiger:::fips2names_state(STATE, "state"),
                        abbr = ggtiger:::fips2names_state(STATE, "abbr"),
                        county = COUNTY, ZCTA5)] %>%
    .[, county := ggtiger:::fips2names_county(state, county)] %>%
    .[, state := NULL] %>%
    .[, .(ZCTA5 = unique(ZCTA5)), by = .(abbr, county)]

save(all_zipcode, file = "data/all_zipcode.RData")
