#' @title Set up sugar bait coverage function
#' @description Set up sugar bait coverage function
#' @inheritParams SugarBaitCoverage
#' @return a **`ramp.xds`** model object
#' @export
SugarBaitCoverage.func <- function(t, xds_obj) {with(xds_obj$sugar_bait_coverage,{
  xds_obj$vars$sugar_bait_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
})}

#' @title Set up dynamic sugar bait coverage function
#' @description Set up a dynamic sugar bait
#' coverage function
#' @inheritParams setup_sugar_bait_coverage
#' @export
setup_sugar_bait_coverage.func = function(name, xds_obj, opts=list()){
  setup_sugar_bait_coverage_func(xds_obj, opts())
}

#' @title Set up dynamic sugar bait coverage function
#' @description Set up dynamic sugar bait coverage function
#' @param xds_obj a **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean sugar_bait_coverage
#' @param F_season the seasonal signal in sugar_bait coverage
#' @param F_trend a temporal trend in sugar_bait coverage
#' @return an **`xds`** object
#' @export
setup_sugar_bait_coverage_func = function(xds_obj, opts=list(),
                                      mean=0,
                                      F_season=F_flat,
                                      F_trend=F_flat){
  xds_obj = dynamic_vector_control(xds_obj)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  coverage$F_trend <- F_trend
  xds_obj$sugar_baits$coverage <- coverage
  return(xds_obj)
}
