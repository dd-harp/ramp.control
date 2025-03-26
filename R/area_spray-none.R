
#' @title No Area Spraying
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return [list]
#' @export
AreaSpray.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no Area Spraying
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
setup_no_area_spray <- function(pars) {
  NoAreaSpray <- list()
  NoAreaSpray$class <- 'none'
  class(NoAreaSpray) <- 'none'
  pars$area_spray <- NoAreaSpray
  pars$area_spray$coverage <- NoAreaSpray
  pars$area_spray$effectsizes <- list()
  pars$area_spray$effectsizes[[1]] <- NoAreaSpray
  return(pars)
}

#' @title Set no spray_area
#' @description The null model for spray_area
#' @inheritParams SprayArea
#' @return [list]
#' @export
SprayArea.none <- function(t, pars) {
  return(pars)
}

#' @title Set up "no spray_area"
#' @inheritParams setup_spray_area
#' @return an **`xds`** object
#' @export
setup_spray_area.none <- function(name, pars, opts=list()) {
  spray_area <- 'none'
  class(spray_area) <- 'none'
  spray_area$class <- 'none'
  pars$area_spray$spray <- spray_area
  return(pars)
}

#' @title Set no area spray effects
#' @description The null model for AreaSprayEffectSizes
#' @inheritParams AreaSprayEffectSizes
#' @return [list]
#' @export
AreaSprayEffects.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_effects
#' @return an **`xds`** object
#' @export
setup_area_spray_effects.none <- function(name='none', pars, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  effects$class <- 'none'
  pars$area_spray$effects <- effects
  return(pars)
}

#' @title Set no AreaSprayCoverage
#' @description The null model for AreaSprayCoverage
#' @inheritParams AreaSprayCoverage
#' @return [list]
#' @export
AreaSprayCoverage.none <- function(t, pars) {
  return(pars)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_coverage
#' @return an **`xds`** object
#' @export
setup_area_spray_coverage.none <- function(name='none', pars, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  coverage$class <- 'none'
  pars$area_spray$coverage <- coverage
  return(pars)
}

#' @title Set no AreaSprayEffectSizes
#' @description The null model for AreaSprayEffectSizes
#' @inheritParams AreaSprayEffectSizes
#' @return [list]
#' @export
AreaSprayEffectSizes.none <- function(t, pars, s) {
  return(pars)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_effects
#' @return an **`xds`** object
#' @export
setup_area_spray_effectsizes.none <- function(name='none', pars, opts=list()) {
  effect_sizes <- 'none'
  class(effect_sizes) <- 'none'
  effect_sizes$class <- 'none'
  pars$area_spray$effect_sizes <- list()
  pars$area_spray$effect_sizes[[1]] <- list()
  return(pars)
}




