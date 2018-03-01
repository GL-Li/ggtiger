# get center and bounding box (bbox) from data

#' get coordinate of center from data
#'
#' @param df: data frame with two columns lon (or long) and lat, usually
#' generated from a shape file.
#'
#' @return a vector of coordinate c(long, lat)
#'
#' @export
#'

get_center <- function(df){
    if (is.null(df$long)){
        df$long <- df$lon
    }
    x <- (min(df$long) + max(df$long)) / 2
    y <- (min(df$lat) + max(df$lat)) / 2

    c(x, y)
}


#' get bounding box from data
#'
#' @param df: data frame with two columns lon (or long) and lat, usually
#' generated from a shape file.
#'
#' @return a vector of bounding box c(left, bottom, right, top)
#'
#' @export
#'

get_bbox <- function(df){
    if (is.null(df$long)){
        df$long <- df$lon
    }
    left <- min(df$long)
    bottom <- min(df$lat)
    right <- max(df$long)
    top <- max(df$lat)

    c(left, bottom, right, top)
}


#' Get right zoom level for map range of data
#'
#' @param df: data frame with two columns lon (or long) and lat, usually
#' generated from a shape file.
#'
#' @return an integer
#'
#' @export
#'

get_zoom <- function(df){
    if (is.null(df$long)){
        df$long <- df$lon
    }
    delta_x <- max(df$long) - min(df$long)
    delta_y <- max(df$lat) - min(df$lat)
    center <- get_center(df)
    message("to be developed")
    NULL
}

