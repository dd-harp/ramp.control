#' @title Set no lsm_effects
#' @description The null model for lsm_effects
#' @inheritParams LSMEffects
#' @return a **`ramp.xds`** model object
#' @export
LSMEffects.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no lsm_effects"
#' @inheritParams setup_lsm_effects
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm_effects.none <- function(name, xds_obj, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  xds_obj$lsm$effects <- effects
  return(xds_obj)
}
