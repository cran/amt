% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_movement2.R
\name{dispersal_kernel}
\alias{dispersal_kernel}
\title{Create a dispersal kernel}
\usage{
dispersal_kernel(
  formula,
  coefs,
  habitat = NULL,
  other.vars = NULL,
  start,
  max.dist,
  init.dir = amt::as_rad(45),
  standardize = TRUE,
  raster = TRUE,
  stop = 0
)
}
\arguments{
\item{formula}{\verb{[formula]} \cr The formula for the dispersal kernel.}

\item{coefs}{\verb{[named numeric]\{>1\}} \cr Coefficients for the terms in the formula. Names of the coefficients must match the name of the terms.}

\item{habitat}{\verb{[RasterLayer]} \cr The habitat matrix / landscape.}

\item{other.vars}{\verb{[data.frame = NULL]} \cr Possible other covariates.}

\item{start}{\verb{[numeric(2)]} \cr Coordinates of the start position.}

\item{max.dist}{\verb{[numeric(1)]} \cr The maximum distance of the dispersal kernel.}

\item{init.dir}{\verb{[numeric(1)]} \cr The initial direction in rad.}

\item{standardize}{\verb{[logical(1) = TRUE]} \cr Should the result be standardized.}

\item{raster}{\verb{[logical(1) = TRUE]} \cr Should a \code{RasterLayer} be returned.}

\item{stop}{\verb{[integer(1)=1]\{0,1\}} \cr What happens when the animal steps out of the landscape.}
}
\description{
Create a dispersal kernel
}
