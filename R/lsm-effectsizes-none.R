
#' @title Set no LSMEffectSizes
#' @description The null model for LSMEffectSizes
#' @inheritParams LSMEffectSizes
#' @return an **`xds`** object
#' @export
LSMEffectSizes.none <- function(t, pars, s) {
  return(pars)
}


#' @title Set up "no lsm_effectsizes"
#' @inheritParams setup_lsm_effectsizes
#' @return an **`xds`** object
#' @export
setup_lsm_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- list()
  class(effectsizes) <- 'none'
  effectsizes$class <- 'none'
  pars$lsm$effectsizes <- list()
  pars$lsm$effectsizes[[1]] <- effectsizes
  return(pars)
}
