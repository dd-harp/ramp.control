
#' @title Set the lsm_effects_lsm
#' @description Set the value of exogenous variables related to
#' lsm_effects_lsm
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
LSMEffects <- function(t, xds_obj) {
  UseMethod("LSMEffects", xds_obj$lsm$effects)
}


#' @title Set up dynamic lsm
#' @description If dynamic lsm has not
#' already been set up, then turn on dynamic
#' lsm and set all the
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm_effects = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_effects", name)
}
