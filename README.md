[![Build Status](https://travis-ci.org/GL-Li/ggtiger.svg?branch=master)](https://travis-ci.org/GL-Li/ggtiger)

Draw and fill in TIGER boundaries on ggmap
==========================================

This package draws and fills in TIGER boundaries on ggmap with a single function geom\_bounday(). As an extension to ggplot2, geom\_boundary() works similarly to native ggplot2 geom\_xxxx() functions.

Installation and setup
----------------------

### Installation

``` r
devtools::install_github("GL-Li/ggtiger")
```

### Setup

This package dowloads TIGER shape files into R with package `tigris`, converts them into `.csv` files and store them in your own computer. You need to create a directory to store the processed data. Let's call the folder `my_tiger_data` and assume the full path to this folder is `xxxxx/my_tiger_data`. Run the function below to set the path for the package.

``` r
set_path_to_tiger("xxxxx/my_tiger_data")
```

Examples
--------

By default, `geom_boundary()` draws all boundaries of a geography in the view of a ggmap. We can also choose to draw only those boundaries in a state, or in selected counties in a state within the map view. \#\#\# Draw boundaries

``` r
library(ggtiger)
ri <- get_map("scituate, RI, united states", zoom = 10, color = "bw")
ggmap(ri) +
    # tracts in two counties in Rhode Islan
    geom_boundary("tract", state = "RI", county = c("providence", "washington"),
                  fill = NA, color = "orange", size = 0.2) +
    # all counties in Rhode Island
    geom_boundary("county", fill = NA, linetype = "dotted", color = "green", size = 0.3) +
    # all state boundaries
    geom_boundary("state", fill = NA, linetype = "dotted", color = "red", size = 0.5)
```

![](README-unnamed-chunk-4-1.png)

### Fill in boudaries

We can fill in boudaies using argument `data_fill` in `geom_boundary()`. The argument takes a data frame that has `GEOID` as the first column and fill-in values as the second column. The `GEOID` in the ?????? form such as `14000US44009051502`, `05000US25007`, and `06000US2502777010`. If the data frame is extracted using census packages such as `tidycensus` and `totalcensus`, it already has the `GEOID` column.

``` r
library(tidycensus)
library(dplyr)
census_api_key("ab664ab627f56ed01df0b97a25f6f473598a7fec")
# get the median home value in Providence county, RI by tract
home_value <- get_acs("tract", "B25077_001", state = "RI", county = "Providence") %>%
    select(c("GEOID", "estimate"))
ggmap(ri) +
    geom_boundary("tract", data_fill = home_value, 
                  color = "blue", size = 0.5, alpha = 0.8) +
    geom_boundary("state", fill = NA, color = "red") +
    scale_fill_gradient(na.value = NA, low = "cyan", high = "orange") +
    labs(fill = "home_value")
```

![](README-unnamed-chunk-5-1.png)

### Combine with point plot

``` r
library(totalcensus)
sb_black <- read_decennial(
    year = 2010,
    states = "IN",
    table_contents = "black_popul = P0030003",
    areas = "St. Joseph County, IN",
    summary_level = "block"
) %>% 
    .[population != 0] %>%
    # percentage of black population in each block
    .[, black_pct := round(100 * black_popul / population, 2)] %>%
    .[order(black_pct)]
#> Reading IN geographic header record file
#> Reading IN file 01 
#> Reading IN file 03

sb <- get_map("south bend, IN", zoom = 13, color = "bw")
ggmap(sb) +
    geom_point(data = sb_black, alpha = 0.8,
               aes(lon, lat, size = population, color = black_pct)) +
    geom_boundary("tract", fill = NA, color = "orange", size = 0.5) +
    scale_size_area(max_size = 10, breaks = c(10, 100, 200, 500)) +
    scale_color_continuous(low = "green", high = "red")
```

![](README-unnamed-chunk-6-1.png)
