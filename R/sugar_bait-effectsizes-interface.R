
#' @title Set the SugarBaitEffectSizes
#' @description Set the value of exogenous variables related to
#' Sugar Baits
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
SugarBaitEffectSizes <- function(t, pars, s) {
  UseMethod("SugarBaitEffectSizes", pars$sugar_baits$effectsizes[[s]])
}

#' @title Set up sugar baits effect sizes
#' @description
#' Set up sugar baits effect sizes
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_sugar_bait_effectsizes", name)
}

