# county fips and names

library(totalcensus)
library(data.table)
library(magrittr)
library(stringr)
library(maps)
fips_names <- read_acs5year(
    year = 2016,
    states = "RI",
    geo_headers = c("STATE", "COUNTY"),
    summary_level = "county"
) %>%
    .[, .(GEOID, STATE, COUNTY, NAME)] %>%
    # regex see https://goo.gl/3FGmLU
    .[, county := tolower(str_extract(NAME, ".+?(?= County)"))]
