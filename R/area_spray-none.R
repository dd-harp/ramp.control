
#' @title No Area Spraying
#' @description The null model for AreaSpray
#' @inheritParams AreaSpray
#' @return a **`ramp.xds`** model object
#' @export
AreaSpray.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up no Area Spraying
#' @param xds_obj a **`xds`** object
#' @return a **`xds`** object
#' @export
setup_no_area_spray <- function(xds_obj) {
  NoAreaSpray <- list()
  NoAreaSpray$class <- 'none'
  class(NoAreaSpray) <- 'none'
  xds_obj$area_spray <- NoAreaSpray
  xds_obj$area_spray$coverage <- NoAreaSpray
  xds_obj$area_spray$effects <- NoAreaSpray
  xds_obj$area_spray$effectsizes <- list()
  xds_obj$area_spray$effectsizes[[1]] <- NoAreaSpray
  return(xds_obj)
}

#' @title Set no spray_area
#' @description The null model for spray_area
#' @inheritParams SprayArea
#' @return a **`ramp.xds`** model object
#' @export
SprayArea.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no spray_area"
#' @inheritParams setup_spray_area
#' @return a **`xds`** object
#' @export
setup_spray_area.none <- function(name, xds_obj, opts=list()) {
  spray_area <- 'none'
  class(spray_area) <- 'none'
  spray_area$class <- 'none'
  xds_obj$area_spray$spray <- spray_area
  return(xds_obj)
}

#' @title Set no area spray effects
#' @description The null model for AreaSprayEffectSizes
#' @inheritParams AreaSprayEffectSizes
#' @return a **`ramp.xds`** model object
#' @export
AreaSprayEffects.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_effects
#' @return a **`xds`** object
#' @export
setup_area_spray_effects.none <- function(name='none', xds_obj, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  effects$class <- 'none'
  xds_obj$area_spray$effects <- effects
  return(xds_obj)
}

#' @title Set no AreaSprayCoverage
#' @description The null model for AreaSprayCoverage
#' @inheritParams AreaSprayCoverage
#' @return a **`ramp.xds`** model object
#' @export
AreaSprayCoverage.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_coverage
#' @return a **`xds`** object
#' @export
setup_area_spray_coverage.none <- function(name='none', xds_obj, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  coverage$class <- 'none'
  xds_obj$area_spray$coverage <- coverage
  return(xds_obj)
}

#' @title Set no AreaSprayEffectSizes
#' @description The null model for AreaSprayEffectSizes
#' @inheritParams AreaSprayEffectSizes
#' @return a **`ramp.xds`** model object
#' @export
AreaSprayEffectSizes.none <- function(t, xds_obj, s) {
  return(xds_obj)
}

#' @title Set up no area spray
#' @inheritParams setup_area_spray_effects
#' @return a **`xds`** object
#' @export
setup_area_spray_effectsizes.none <- function(name='none', xds_obj, opts=list()) {
  effect_sizes <- 'none'
  class(effect_sizes) <- 'none'
  effect_sizes$class <- 'none'
  xds_obj$area_spray$effect_sizes <- list()
  xds_obj$area_spray$effect_sizes[[1]] <- list()
  return(xds_obj)
}




