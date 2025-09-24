
#' @title Set Up a IRS Coverage Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' coverage
#'
#' @inheritParams setup_irs_coverage
#' @export
setup_irs_coverage.func = function(name="func", xds_obj, options=list()){
  class(xds_obj$irs_obj) = "dynamic"
  xds_obj$irs_obj$cover_obj <- make_irs_coverage_func(options)
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param options a list of options to override defaults
#' @param mean the mean irs_coverage
#' @param F_season the seasonal signal in irs coverage
#' @param season_par parameters to configure F_season
#' @param F_trend a temporal trend in irs coverage
#' @param trend_par parameters to configure F_trend
#'
#' @return a **`ramp.xds`** model object
#' @export
make_irs_coverage_func = function(options=list(),
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


#' @title Set no irs_coverage
#' @description The null model for irs_coverage
#' @inheritParams IRS_Coverage
#' @return a **`ramp.xds`** model object
#' @export
IRS_Coverage.func <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$cover_obj,{
    xds_obj$irs_obj$coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
    return(xds_obj)
  })}
