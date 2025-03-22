
#' @title Set up LSM effect sizes
#' @description Set up effect sizes for LSM
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @param s vector species index
#' @return an **`xds`** object
#' @export
LSMEffectSizes <- function(t, pars, s) {
  UseMethod("LSMEffectSizes", pars$lsm$effectsizes[[s]])
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_lsm_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_lsm_effectsizes", name)
}

