% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning.R
\name{flag_roundtrips}
\alias{flag_roundtrips}
\alias{flag_roundtrips.track_xyt}
\title{Flag Fast Round Trips}
\usage{
flag_roundtrips(x, delta, epsilon, time_unit = "secs", ...)

\method{flag_roundtrips}{track_xyt}(x, delta, epsilon, time_unit = "secs", ...)
}
\arguments{
\item{x}{\verb{[track_xyt]} A \code{track_xyt} object.}

\item{delta}{\verb{[numeric]} The threshold SDR for flagging. Locations that imply
both legs of a round trip with \code{SDR > delta/epsilon} are flagged. See details.}

\item{epsilon}{\verb{[numeric]} A scaling factor to create the threshold for
flagging.}

\item{time_unit}{\verb{[character]} Character string giving time unit. Should be
\code{"secs"}, \code{"mins"}, or \code{"hours"}. See details.}

\item{...}{Addtional arguments. None currently implemented.}
}
\value{
Returns \code{x} (a \code{track_xyt}) with a flagging column added
(\code{x$fast_roundtrip_}).
}
\description{
Flags locations that imply fast round trips
}
\details{
Locations implying a single fast step can be flagged using
\code{\link{flag_fast_steps}()}. However, it is more likely that a single
location is imprecise if it implies an unrealistically fast out-and-back round
trip. In that case, the user might be willing to scale the threshold SDR.
In this function, \code{delta} gives the base SDR and \code{epsilon} is the scaling
factor, such that locations are considered for flagging if the SDR from the
previous location (location i - 1) to the focal location (i) [call it \code{sdr1}]
and the focal location (i) to the next location (i + 1) [call it \code{sdr2}] both
have \code{SDR > delta/epsilon}.

In that case, the SDR from the previous location (i - 1) to the next location
(i + 1) is computed; i.e., the SDR assuming we omit the focal location (i)
[call it \code{sdr3}]. The remaining locations should be closer together than
they are to the omitted location. Thus the focal location is flagged if
\code{(sdr1 > epsilon * sdr3) & (sdr2 > epsilon * sdr3)}.

Note that \code{epsilon} both \emph{decreases} \code{delta} in the out-and-back case and
\emph{increases} \code{sdr3} (between the remaining neighbors).

Internally, flagged locations are dropped from future consideration.

The \code{time_unit} should be the same time unit with which the SDR threshold
was calculated. SDR is typically calculated in \code{m^2/s}, so \code{time_unit} defaults
to \code{"secs"}. The spatial unit is determined by the CRS, which should typically
be in meters. The \code{epsilon} parameter is unitless.
}
\seealso{
\code{\link{flag_duplicates}()},
\code{\link{flag_fast_steps}()},
\code{\link{flag_defunct_clusters}()}
}
\author{
Brian J. Smith, based on code by Johannes Signer and Tal Avgar
}
