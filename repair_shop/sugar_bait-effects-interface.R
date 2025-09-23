
#' @title Compute sugar bait effects
#' @description Compute the effects of a sugar bait
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
SugarBaitEffects <- function(t, xds_obj) {
  UseMethod("SugarBaitEffects", xds_obj$sugar_baits$effects)
}


#' @title Set up a sugar bait effects model
#' @description Set up a sugar bait effects model
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_sugar_bait_effects = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_effects", name)
}
