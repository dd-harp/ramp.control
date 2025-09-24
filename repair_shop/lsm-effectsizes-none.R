
#' @title Set no LSMEffectSizes
#' @description The null model for LSMEffectSizes
#' @inheritParams LSMEffectSizes
#' @return a **`ramp.xds`** model object
#' @export
LSMEffectSizes.none <- function(t, xds_obj, s) {
  return(xds_obj)
}


#' @title Set up "no lsm_effectsizes"
#' @inheritParams setup_lsm_effectsizes
#' @return a **`ramp.xds`** model object
#' @export
setup_lsm_effectsizes.none <- function(name, xds_obj, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  effectsizes$class <- 'none'
  xds_obj$lsm$effectsizes <- list()
  xds_obj$lsm$effectsizes[[1]] <- effectsizes
  return(xds_obj)
}
