
#' @title Set up LSM effect sizes
#' @description Set up effect sizes for LSM
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @param s vector species index
#' @return a **`ramp.xds`** model object
#' @export
LSMEffectSizes <- function(t, xds_obj, s) {
  UseMethod("LSMEffectSizes", xds_obj$lsm$effectsizes[[s]])
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
setup_lsm_effectsizes = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_effectsizes", name)
}

