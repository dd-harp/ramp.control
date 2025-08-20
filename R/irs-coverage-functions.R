#' @title Set no irs_coverage
#' @description The null model for irs_coverage
#' @inheritParams IRSCoverage
#' @return a **`ramp.xds`** model object
#' @export
IRSCoverage.func <- function(t, xds_obj) {with(xds_obj$irs$coverage_mod,{
  xds_obj$irs$coverage = mx*pmin(pmax(0, mx*F_season(t)*F_trend(t)),1)
  return(xds_obj)
})}

#' @title Set up a function for IRS coverage
#'
#' @description A set up utility function for
#' [IRSCoverage]. Set up a s
#'
#' @inheritParams setup_irs_coverage
#'
#' @returns An IRS coverage model object
#'
#' @export
setup_irs_coverage.func = function(name, xds_obj, opts=list()){
  setup_irs_coverage_func(opts)
}

#' @title Set up dynamic forcing
#' @description Set up a simple function
#' to model IRS coverage
#'
#' @param opts a list of options to override defaults
#' @param mx peak irs_coverage
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par an model object to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par an model object to configure a trends function using [make_function]
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_coverage_func = function(opts=list(),
                                   mx = 1,
                                   F_season=F_flat, season_par = list(),
                                   F_trend=F_flat, trend_par = list()){
  with(opts,{
    coverage <- list()
    class(coverage) <- 'func'
    stopifnot(mx<=1 & mx>=0)
    coverage$mx <- mx

    coverage$F_season = F_season
    coverage$season_par <- season_par
    if(length(season_par)>0)
      coverage$F_season <- make_function(season_par)


    coverage$F_trend = F_trend
    coverage$trend_par <- trend_par
    if(length(trend_par)>0)
      coverage$F_trend <- make_function(trend_par)

    return(coverage)
})}
