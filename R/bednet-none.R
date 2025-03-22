
#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet"
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_bednets <- function(pars) {
  bednets <- list()
  bednets$name <- 'none'
  class(bednets) <- 'none'
  pars$bednets <- bednets
  pars$bednets$coverage <- bednets
  pars$bednets$effectsizes <- list()
  pars$bednets$effectsizes[[1]] <- bednets
  return(pars)
}

#' @title Set no distribute_bednets
#' @description The null model for distribute_bednets
#' @inheritParams DistributeBedNets
#' @return [list]
#' @export
DistributeBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no distribute_bednets"
#' @inheritParams setup_distribute_bednets
#' @return an **`xds`** object
#' @export
setup_distribute_bednets.none <- function(name, pars, opts) {
  distribute <- 'none'
  class(distribute) <- 'none'
  pars$bednets$distribute <- distribute
  return(pars)
}

#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams OwnBedNets
#' @return an **`xds`** object
#' @export
OwnBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no own_bednets"
#' @inheritParams setup_own_bednets
#' @return an **`xds`** object
#' @export
setup_own_bednets.none <- function(name, pars, opts) {
  own <- 'none'
  class(own) <- 'none'
  pars$bednets$own <- own
  return(pars)
}

#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams UseBedNets
#' @return [list]
#' @export
UseBedNets.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no use_bednets"
#' @inheritParams setup_use_bednets
#' @return an **`xds`** object
#' @export
setup_use_bednets.none <- function(name, pars, opts) {
  use <- 'none'
  class(use) <- 'none'
  pars$bednets$use <- use
  return(pars)
}

#' @title Set no bednet_effects
#' @description The null model for bednet_effects
#' @inheritParams BedNetEffects
#' @return [list]
#' @export
BedNetEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet_effects"
#' @inheritParams setup_bednet_effects
#' @return an **`xds`** object
#' @export
setup_bednet_effects.none <- function(name, pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  pars$bednets$effects <- effects
  return(pars)
}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNetCoverage
#' @return [list]
#' @export
BedNetCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no bednet_coverage"
#' @inheritParams setup_bednet_coverage
#' @return an **`xds`** object
#' @export
setup_bednet_coverage.none <- function(name, pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  pars$bednets$coverage <- coverage
  return(pars)
}

#' @title Set no bednet_effectsizes
#' @description The null model for bednet_effectsizes
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up "no bednet_effectsizes"
#' @inheritParams setup_bednet_effectsizes
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes.none <- function(name, pars, opts) {
  effectsizes <- 'none'
  class(effectsizes) <- 'none'
  pars$bednets$effectsizes <- effectsizes
  return(pars)
}

