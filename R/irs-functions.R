#' @title Set no irs_coverage
#' @description The null model for irs_coverage
#' @inheritParams IRSCoverage
#' @return an **`xds`** object
#' @export
IRSCoverage.func <- function(t, pars) {with(pars$irs$coverage,{
  pars$vars$irs_coverage = mx*pmin(pmax(0, mx*F_season(t)*F_trend(t)),1)
  return(pars)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_coverage
#' @export
setup_irs_coverage.func = function(name, pars, opts=list()){
  setup_irs_coverage_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mx peak irs_coverage
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par an object to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par an object to configure a trends function using [make_function]
#' @return an **`xds`** object
#' @export
setup_irs_coverage_func = function(pars, opts=list(),
                                   mx = 1,
                                   F_season=F_flat, season_par = list(),
                                   F_trend=F_flat, trend_par = list()){
  with(opts,{
    pars = dynamic_vector_control(pars)
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
    pars$irs$coverage <- coverage
    return(pars)
})}
