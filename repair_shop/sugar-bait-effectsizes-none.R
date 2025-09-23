
#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitEffectSizes
#' @return [list]
#' @export
SugarBaitEffectSizes.none <- function(t, xds_obj, s) {
  return(xds_obj)
}


#' @title Set up "no sugar_bait_effectsizes"
#' @inheritParams setup_sugar_bait_effectsizes
#' @return a **`ramp.xds`** model object
#' @export
setup_sugar_bait_effectsizes.none <- function(name, xds_obj, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  xds_obj$sugar_bait$effectsizes <- effectsizes
  return(xds_obj)
}
