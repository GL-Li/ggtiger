% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_boundary.R
\name{geom_boundary}
\alias{geom_boundary}
\title{Draw census boudaries on ggmap}
\usage{
geom_boundary(geography, data_fill = NULL, year = 2016, states = NULL,
  counties = NULL, N = 100, mapping = NULL, data = NULL,
  geom = "polygon", position = "identity", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...)
}
\arguments{
\item{geography}{geography of which boudaries to be drawn. Currently take
"state", "county", "county subdivision", "tract", "block group".}

\item{data_fill}{data frame to fill in the boundaries. It has GEOID as the
first column and the values to fill as the second column.}

\item{year}{year of the shape files updated.}

\item{states}{abbreviation of a state within which boudaries are drawn, such as "RI".}

\item{counties}{vector of county names of the state, such as c("providence", "kent")}

\item{N}{integer when state is NULL, N x N grid points on the view of map are
used to determine the states and counties within the view.}

\item{mapping}{same as those in ggplot functions, typically inherited from ggmap()}

\item{data}{data typically inherited from ggmap().}

\item{geom}{same as those in ggplot2 functions but should be uisng "polygon".}

\item{position}{same as those in ggplot2 functions.}

\item{na.rm}{same as those in ggplot2 functions.}

\item{show.legend}{same as those in ggplot2 functions.}

\item{inherit.aes}{same as those in ggplot2 functions.}

\item{...}{same as those in ggplot2 functions.}
}
\value{
a ggplot2 object
}
\description{
Draw census boudaries on ggmap
}
