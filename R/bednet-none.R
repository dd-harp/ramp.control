
#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNet
#' @return [list]
#' @export
BedNet.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no bednet"
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_bednets <- function(xds_obj) {
  xds_obj$bednets <- none_obj()
  xds_obj$bednets$distribute_mod <- none_obj()
  xds_obj$bednets$owner_mod <- none_obj()
  xds_obj$bednets$user_mod <- none_obj()
  xds_obj$bednets$effects_mod <- none_obj()
  xds_obj$bednets$coverage_mod <- none_obj()
  xds_obj$bednets$ef_sz_mod <- list()
  xds_obj$bednets$ef_sz_mod[[1]] <- none_obj()
  return(xds_obj)
}

#' @title Set no distribute_bednets
#' @description The null model for distribute_bednets
#' @inheritParams DistributeBedNets
#' @return [list]
#' @export
DistributeBedNets.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no distribute_bednets"
#' @inheritParams setup_distribute_bednets
#' @return a **`ramp.xds`** model object
#' @export
setup_distribute_bednets.none <- function(name, xds_obj, opts) {
  return(none_obj())
}

#' @title Set no own_bednets
#' @description The null model for own_bednets
#' @inheritParams OwnBedNets
#' @return a **`ramp.xds`** model object
#' @export
OwnBedNets.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no own_bednets"
#' @inheritParams setup_own_bednets
#' @return a **`ramp.xds`** model object
#' @export
setup_own_bednets.none <- function(name, xds_obj, opts) {
  return(none_obj())
}

#' @title Set no use_bednets
#' @description The null model for use_bednets
#' @inheritParams UseBedNets
#' @return [list]
#' @export
UseBedNets.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no use_bednets"
#' @inheritParams setup_use_bednets
#' @return a **`ramp.xds`** model object
#' @export
setup_use_bednets.none <- function(name, xds_obj, opts) {
  return(none_obj())
}

#' @title Set no bednet_effects
#' @description The null model for bednet_effects
#' @inheritParams BedNetEffects
#' @return [list]
#' @export
BedNetEffects.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no bednet_effects"
#' @inheritParams setup_bednet_effects
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_effects.none <- function(name, xds_obj, opts=list()) {
  return(none_obj())
}

#' @title Set no bednet
#' @description The null model for bednet
#' @inheritParams BedNetCoverage
#' @return [list]
#' @export
BedNetCoverage.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no bednet_coverage"
#' @inheritParams setup_bednet_coverage
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_coverage.none <- function(name, xds_obj, opts=list()) {
  return(none_obj())
}

#' @title Set no bednet_effect_sizes
#' @description The null model for bednet_effect_sizes
#' @inheritParams BedNetEffectSizes
#' @return [list]
#' @export
BedNetEffectSizes.none <- function(t, xds_obj, s) {
  return(xds_obj)
}

#' @title Set up "no bednet_effect_sizes"
#' @inheritParams setup_bednet_effect_sizes
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_effect_sizes.none <- function(name, xds_obj, opts) {
  return(none_obj())
}

