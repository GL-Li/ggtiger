% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_center_and_bbox_from_data.R
\name{get_bbox}
\alias{get_bbox}
\title{get bounding box from data}
\usage{
get_bbox(df)
}
\arguments{
\item{df}{data frame with two columns lon (or long) and lat, usually
generated from a shape file.}
}
\value{
a vector of bounding box c(left, bottom, right, top)
}
\description{
get bounding box from data
}
