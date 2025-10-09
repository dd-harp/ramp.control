
#' @title Set Up a Bed Net Coverage Function
#'
#' @description
#' This sets up a function to model bed net coverage.
#'
#' + A function F_cover
#'
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.func = function(name="func", xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$bednet_obj) = "dynamic"
  xds_obj$bednet_obj$cover_obj <- make_bednet_coverage_function(options)
  xds_obj$bednet_obj$coverage = rep(0, xds_obj$nPatches)
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param options a list of options to override defaults
#' @param mean the mean bednet_coverage
#' @param F_season the seasonal signal in bednet coverage
#' @param season_par parameters to configure F_season
#' @param F_trend a temporal trend in bednet coverage
#' @param trend_par parameters to configure F_trend
#'
#' @return a **`ramp.xds`** model object
#' @export
make_bednet_coverage_function = function(options=list(),
                                     mean=1,
                                     F_season=F_flat,
                                     season_par=list(),
                                     F_trend=F_flat,
                                     trend_par=list()){
  with(options,{
    coverage <- list()
    class(coverage) <- 'func'
    coverage$name <- 'func'
    coverage$mean <- mean
    coverage$F_season <- F_season
    if(length(season_par)>1){
      coverage$season_par <- season_par
      coverage$F_season <- make_function(season_par)
    }
    coverage$F_trend <- F_trend
    if(length(trend_par)>1){
      coverage$trend_par <- trend_par
      coverage$F_trend <- make_function(trend_par)
    }
    return(coverage)
  })}


#' @title Bed Net Coverage
#'
#' @description This computes two quantities:
#'
#' + `effective_coverage`
#'
#' + `effective_contact`
#'
#' @inheritParams Bed_Net_Coverage
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Coverage.func <- function(t, y, xds_obj) {
  with(xds_obj$bednet_obj$cover_obj,{
    coverage <- pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
    xds_obj$bednet_obj$coverage = coverage
  return(xds_obj)
})}
