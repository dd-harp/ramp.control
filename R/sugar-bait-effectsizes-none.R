
#' @title No sugar baits
#' @description The null model for sugar baits
#' @inheritParams SugarBaitEffectSizes
#' @return [list]
#' @export
SugarBaitEffectSizes.none <- function(t, pars, s) {
  return(pars)
}


#' @title Set up "no sugar_bait_effectsizes"
#' @inheritParams setup_sugar_bait_effectsizes
#' @return an **`xds`** object
#' @export
setup_sugar_bait_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  pars$sugar_bait$effectsizes <- effectsizes
  return(pars)
}
