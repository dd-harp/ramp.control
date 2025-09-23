

#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitCoverage
#' @return a **`ramp.xds`** model object
#' @export
SugarBaitCoverage.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up no sugar bait coverage
#' @inheritParams setup_sugar_bait_coverage
#' @return a **`ramp.xds`** object
#' @export
setup_sugar_bait_coverage.none <- function(name='none', xds_obj, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  xds_obj$sugar_baits$coverage <- coverage
  return(xds_obj)
}
