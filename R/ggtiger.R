#' Draw and fill in TIGER boundaries on ggmap
#'
#'Draw and fill in TIGER boundaries on ggmap with a single function
#' geom_bounday(). As an extension to ggplot2, geom_boundary() works similarly
#' to native ggplot2 geom_xxxx() functions.
#'
#' @import ggmap
#' @import data.table
#' @import maps
#' @import tigris
#' @importFrom magrittr "%>%"
#' @importFrom rgdal readOGR
#' @importFrom broom tidy
#' @importFrom stringr str_extract str_replace str_trim
#' @importFrom utils menu read.table write.table

NULL
