

#' @title Set no LSMCoverage
#' @description The null model for LSMCoverage
#' @inheritParams LSMCoverage
#' @return a **`ramp.xds`** model object
#' @export
LSMCoverage.none <- function(t, xds_obj) {
  return(xds_obj)
}


#' @title Set up "no lsm_coverage"
#' @inheritParams setup_lsm_coverage
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm_coverage.none <- function(name='none', xds_obj, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  xds_obj$lsm$coverage <- coverage
  return(xds_obj)
}
