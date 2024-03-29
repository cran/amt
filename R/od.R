#' Occurrence Distribution
#'
#' `od` is a wrapper around `ctmm::occurrence`. See `help(ctmm::occurrence)` for more details. `rolling_od` estimates occurrence distributions for a subset of a track.
#'
#'
#' @template track_xyt
#' @param trast `[SpatRaster]` \cr A template raster for the extent and resolution of the result.
#' @param model `[An output of fit_ctmm]` \cr The autocorrelation model that should be fit to the data. `bm` corresponds to Brownian motion, `ou` to an Ornstein-Uhlenbeck process, `ouf` to an Ornstein-Uhlenbeck forage process.
#' @param res.space `[numeric(1)=10]` \cr Number of grid point along each axis, relative to the average diffusion (per median timestep) from a stationary point. See also `help(ctmm::occurrence)`.
#' @param res.time `[numeric(1)=10]` \cr Number of temporal grid points per median timestep.
#' @param n.points `[numeric(1)=5]` \cr This argument is only relevant for `rolling_od` and specifies the window size for the od estimation.
#' @param show.progress `[logical(1)=TRUE]` \cr Indicates if a progress bar is used.
#' @template dots_none
#' @references Fleming, C. H., Fagan, W. F., Mueller, T., Olson, K. A., Leimgruber, P., & Calabrese, J. M. (2016). Estimating where and how animals travel: an optimal framework for path reconstruction from autocorrelated tracking data. Ecology.
#' @name od
#' @examples
#'
#' \donttest{
#' data(deer)
#' mini_deer <- deer[1:100, ]
#' trast <- make_trast(mini_deer)
#' md <- od(mini_deer, trast = trast)
#' terra::plot(md)
#'
#' # rolling ud
#' xx <- rolling_od(mini_deer, trast)
#' }

#' @export
#' @rdname od
rolling_od <- function(x, ...) {
  UseMethod("rolling_od", x)
}

#' @export
#' @rdname od
rolling_od.track_xyt <- function(
  x, trast, model = fit_ctmm(x, "bm"), res.space = 10, res.time = 10,
  n.points = 5, show.progress = TRUE, ...) {
  res <- list()

  if (show.progress) {
    pb <- utils::txtProgressBar(style = 3, max = nrow(x), min = n.points + 1)
  }
  for (i in (n.points + 1):nrow(x)) {
    if (show.progress) utils::setTxtProgressBar(pb, i)
    j <- i - n.points
    suppressWarnings(res[[j]] <- od(x[j:i, ], trast = trast, model = model,
                                    res.space = res.space, res.time = res.time))
  }
  res <- terra::rast(res)
  terra::time(res) <-  x$t_[(n.points + 1):nrow(x)]
  res
}


#' @export
#' @rdname od
od <- function(x, ...) {
  UseMethod("od", x)
}

#' @export
#' @rdname od
od.track_xyt <- function(x, trast,
                         model = fit_ctmm(x, "bm"),
                         res.space = 10, res.time = 10, ...) {

  if (is.na(terra::crs(trast) == "")) {
    stop("trast, needs a cooridnate reference system (crs).")
  }
  krige <- ctmm::occurrence(as_telemetry(x),
                            CTMM = model, res.space = res.space, res.time = res.time)

  r <- 1 - terra::rast(ctmm::raster(krige, DF = "CDF"))
  r <- terra::project(r, y = trast, method = "bilinear", gdal = TRUE,
                      res = terra::res(trast))
  r <- terra::resample(r, trast) # needed?
  r <- terra::ifel(is.na(r), 0, r)
  attr(r, "model") <- model
  r
}



