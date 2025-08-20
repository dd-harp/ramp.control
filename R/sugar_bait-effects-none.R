#' @title Set up no sugar bait effects
#' @description Set up no sugar bait effects
#' @inheritParams SugarBaitEffects
#' @return a **`ramp.xds`** model object
#' @export
SugarBaitEffects.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up no sugar bait effects
#' @inheritParams setup_sugar_bait_effects
#' @return a **`ramp.xds`** model object
#' @export
setup_sugar_bait_effects.none <- function(name, xds_obj, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  xds_obj$sugar_baits$effects <- effects
  return(xds_obj)
}
