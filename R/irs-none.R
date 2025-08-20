
#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRS
#' @return a **`ramp.xds`** model object
#' @export
IRS.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no irs"
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_no_irs <- function(xds_obj) {
  no_irs <- list()
  class(no_irs) = 'none'
  no_irs$name = 'none'
  xds_obj$irs <- no_irs
  xds_obj$irs$spray_mod <- no_irs
  xds_obj$irs$effects_mod <- no_irs
  xds_obj$irs$coverage_mod <- no_irs

  xds_obj$irs$ef_sz_mod <- list()
  xds_obj$irs$ef_sz_mod[[1]] <- no_irs
  return(xds_obj)
}

#' @title Set no spray_houses
#' @description The null model for spray_houses
#' @inheritParams SprayHouses
#' @return [list]
#' @export
SprayHouses.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no spray_houses"
#' @inheritParams setup_spray_houses
#' @return a irs spray model object
#' @export
setup_spray_houses.none <- function(name, xds_obj, opts=list()) {
  spray_houses <- list()
  spray_houses$name <- "none"
  class(spray_houses) <- "none"
  return(spray_houses)
}

#' @title Set no irs_effects
#' @description The null model for irs_effects
#' @inheritParams IRSEffects
#' @return a **`ramp.xds`** model object
#' @export
IRSEffects.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no irs_effects"
#' @inheritParams setup_irs_effects
#' @return a irs effects model object
#' @export
setup_irs_effects.none <- function(name, xds_obj, opts=list()) {
  effects <- 'none'
  class(effects) <- 'none'
  return(effects)
}


#' @title Set no irs
#' @description The null model for irs
#' @inheritParams IRSCoverage
#' @return a **`ramp.xds`** model object
#' @export
IRSCoverage.none <- function(t, xds_obj) {
  return(xds_obj)
}

#' @title Set up "no irs_coverage"
#' @inheritParams setup_irs_coverage
#' @return a irs coverage model object
#' @export
setup_irs_coverage.none <- function(name='none', xds_obj, opts=list()) {
  coverage <- 'none'
  class(coverage) <- 'none'
  return(coverage)
}


#' @title Set no irs_effectsizes
#' @description The null model for irs_effectsizes
#' @inheritParams IRSEffectSizes
#' @return a **`ramp.xds`** model object
#' @export
IRSEffectSizes.none <- function(t, xds_obj, s) {
  return(xds_obj)
}

#' @title Set up "no irs_effectsizes"
#' @inheritParams setup_irs_effect_sizes
#' @return a irs effect size model object
#' @export
setup_irs_effect_sizes.none <- function(name, xds_obj, opts) {
  effectsizes <- list()
  effectsizes$class <- 'none'
  class(effectsizes) <- 'none'
  ef_sz_mod <- list()
  ef_sz_mod[[1]] <- effectsizes
  return(ef_sz_mod)
}
