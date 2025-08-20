
#' @title Set the LSMCoverage
#' @description Set the value of exogenous variables related to
#' LSMCoverage
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
LSMCoverage <- function(t, xds_obj) {
  UseMethod("LSMCoverage", xds_obj$lsm$coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm_coverage = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_coverage", name)
}


