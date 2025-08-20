#' @title Set no lsm_coverage
#' @description The null model for lsm_coverage
#' @inheritParams LSMCoverage
#' @return [list]
#' @export
LSMCoverage.func <- function(t, xds_obj) {with(xds_obj$lsm_coverage,{
  xds_obj$vars$lsm_coverage = pmin(pmax(0, mean*F_season(t)*F_trend(t)),1)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_lsm_coverage
#' @export
setup_lsm_coverage.func = function(name, xds_obj, opts=list()){
  setup_lsm_coverage_func(xds_obj, opts())
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj a **`xds`** object
#' @param opts a list of options to override defaults
#' @param mean the mean lsm_coverage
#' @param F_season the seasonal signal in lsm coverage
#' @param F_trend a temporal trend in lsm coverage
#' @return a **`xds`** object
#' @export
setup_lsm_coverage_func = function(xds_obj, opts=list(),
                                      mean=0,
                                      F_season=F_flat,
                                      F_trend=F_flat){
  xds_obj = dynamic_vector_control(xds_obj)
  coverage <- list()
  class(coverage) <- 'func'
  coverage$mean <- mean
  coverage$F_season <- F_season
  coverage$F_trend <- F_trend
  xds_obj$lsm$coverage <- coverage
  return(xds_obj)
}
