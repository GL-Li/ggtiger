# AK and HI county fips are not in the maps package

library(totalcensus)
library(data.table)
library(magrittr)
library(stringr)

# AK and HI from 2016 5-year estimate
ak_hi <- read_acs5year(
    year = 2016,
    state = c("HI", "AK"),
    geo_headers = c("COUNTY", "STATE", "NAME"),
    summary_level = "county"
) %>%
    .[, .(fips = COUNTY,
          state = fips2names_state(STATE, "state"),
          county = str_extract(NAME, ".+?(?= County| Borough| Census Area| City and Borough| Municipality)"))] %>%
    .[, county := tolower(county)] %>%
    # Kusilvak is not in 2010 census
    .[20, ":=" (fips = "158", state = "alaska", county = "kusilvak")]

# continental USA from maps package
maps_fips <- maps::county.fips %>%
    setDT() %>%
    .[, fips := str_extract(fips, ".{3}$")] %>%
    .[, state := str_extract(polyname, "^[^,]*")] %>%
    .[, county := str_extract(polyname, "[^,]*$")] %>%
    .[, polyname := NULL] %>%
    # county such as "san juan:lopez island" and "san juan:orcas island"
    # has the same key (state, fips), keep only one of them
    .[, county := str_extract(county, "^[^:]*")] %>%
    unique() %>%
    # still one duplicates "067, montana, park" and
    # "067, montana, yellowstone national", keep only the later
    .[!(fips == "067" & state == "montana" & county == "park")]

all_county_fips <- rbindlist(list(ak_hi, maps_fips))
save(all_county_fips, file = "data/all_county_fips.RData")
