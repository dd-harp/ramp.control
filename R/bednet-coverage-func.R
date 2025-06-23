#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams BedNetCoverage
#' @return [list]
#' @export
BedNetCoverage.func <- function(t, pars) {with(pars$bednets$coverage,{
  pars$vars$bednet_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
  return(pars)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.func = function(name, pars, opts=list()){
  setup_bednet_coverage_func(pars, opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean bednet_coverage
#' @param F_season the seasonal signal in bednet coverage
#' @param season_par a parameter set for [F_season] using [make_function]
#' @param F_trend a temporal trend in bednet coverage
#' @param trend_par a parameter set for [F_trend] using [make_function]
#' @return an **`xds`** object
#' @export
setup_bednet_coverage_func = function(pars, opts=list(),
                                      mean=1,
                                      F_season=F_flat,
                                      season_par= list(),
                                      F_trend=F_flat,
                                      trend_par = list()){with(opts,{
  browser()
  pars = dynamic_vector_control(pars)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$name <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  if(length(season_par)>0){
    coverage$
    coverage$F_season <- make_function(season_par)
  }
  coverage$F_trend <- F_trend
  if(length(trend_par)>0){
    coverage$F_trend <- make_function(trend_par)
  }
  pars$bednets$coverage <- coverage
  return(pars)
})}
