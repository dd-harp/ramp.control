
#' @title Set the SugarBaitEffectSizes
#' @description Set the value of exogenous variables related to
#' Sugar Baits
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`** model object
#' @param s vector species index
#' @return a **`ramp.xds`** model object
#' @export
SugarBaitEffectSizes <- function(t, xds_obj, s) {
  UseMethod("SugarBaitEffectSizes", xds_obj$sugar_baits$effectsizes[[s]])
}

#' @title Set up sugar baits effect sizes
#' @description
#' Set up sugar baits effect sizes
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`** model object
#' @param opts a list of options to override defaults
#' @return a **`ramp.xds`** model object
#' @export
setup_sugar_bait_effectsizes = function(name, xds_obj, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_effectsizes", name)
}

