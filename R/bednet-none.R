
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
  pars$bednets <- none_obj()
  pars$bednets$distribute_mod <- none_obj()
  pars$bednets$owner_mod <- none_obj()
  pars$bednets$user_mod <- none_obj()
  pars$bednets$effects_mod <- none_obj()
  pars$bednets$coverage_mod <- none_obj()
  pars$bednets$ef_sz_mod <- list()
  pars$bednets$ef_sz_mod[[1]] <- none_obj()
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
  return(none_obj())
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
  return(none_obj())
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
  return(none_obj())
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
  return(none_obj())
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
  return(none_obj())
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
  return(none_obj())
}

