% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track_resample.R
\name{filter_min_n_burst}
\alias{filter_min_n_burst}
\alias{filter_min_n_burst.track_xy}
\title{Filter bursts by number of relocations}
\usage{
filter_min_n_burst(x, ...)

\method{filter_min_n_burst}{track_xy}(x, min_n = 3, ...)
}
\arguments{
\item{x}{\code{[track_xy, track_xyt]} \cr A track created with \code{make_track}.}

\item{...}{Further arguments, none implemented.}

\item{min_n}{\verb{[numeric(1)=3]} \cr Indicating the minimum number of relocations (=fixes per burst).}
}
\value{
A \code{tibble} of class \code{track_xy(t)}.
}
\description{
Only retain bursts with a minimum number (= \code{min_n}) of relocations.
}
