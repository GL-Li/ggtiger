#' Modified ggmap::ggmap() function
#'
#' The extent is default to "normal" and the xlim and ylim is set in `coord_map()` so
#' they will not delete data outside the limits in added ggplot layers. All others are the
#' same as ggmap::ggmap().
#'
#'
#' @param ggmap an object of class ggmap (from function get_map)
#' @param extent how much of the plot should the map take up?
#'   "normal" (default), "device", or "panel"
#' @param base_layer a ggplot(aes(...), ...) call; see examples
#' @param maprange logical for use with base_layer; should the map
#'   define the x and y limits?
#' @param legend "left", "right" (default), "bottom", "top",
#'   "bottomleft", "bottomright", "topleft", "topright", "none"
#'   (used with extent = "device")
#' @param padding distance from legend to corner of the plot (used
#'   with legend, formerly b)
#' @param darken vector of the form c(number, color), where number
#'   is in [0, 1] and color is a character string indicating the
#'   color of the darken.  0 indicates no darkening, 1 indicates a
#'   black-out.
#' @param b Deprecated, renamed to `padding`. Overrides any
#'   `padding` argument.
#' @param fullpage Deprecated, equivalent to `extent = "device"`
#'   when `TRUE`. Overrides any `extent` argument.
#' @param expand Deprecated, equivalent to `extent = "panel"`
#'   when `TRUE` and `fullpage` is `FALSE`. When `fullpage`
#'   is `FALSE` and `expand` is `FALSE`, equivalent to
#'   `extent="normal"`. Overrides any `extent` argument.
#' @param ... ...
#'
#' @return a ggplot object
#'
#' @export


ggmap <- function(ggmap, extent = "normal", base_layer, maprange = FALSE,
                  legend = "right", padding = 0.02, darken = c(0, "black"), ...){
    ggmap::ggmap(ggmap, extent, base_layer, maprange,
          legend, padding, darken, ...) +
        coord_map(projection="mercator",
                  xlim=c(attr(ggmap, "bb")$ll.lon, attr(ggmap, "bb")$ur.lon),
                  ylim=c(attr(ggmap, "bb")$ll.lat, attr(ggmap, "bb")$ur.lat))
}
