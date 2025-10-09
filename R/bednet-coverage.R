
#' @title Set Up the Bed Net Coverage Object
#'
#' @description Set up an object to model
#' bed net effective coverage, which is defined
#' as the product of access and use.
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_coverage = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_bednet_coverage", name)
}

#' @title Set Up the Null Bed Net Coverage Object
#'
#' @description
#' Set up the null bed net coverage object
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_bednet_coverage.none = function(name, xds_obj, options=list()){
  xds_obj$bednet_obj$cover_obj = make_none_object()
  xds_obj$bednet_obj$coverage = rep(0, xds_obj$nPatches)
  return(xds_obj)
}

#' @title Get bednet coverage
#'
#' @description Get the values from a model
#' that set maximum bednet coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
get_bednet_coverage = function(xds_obj){
  xds_obj$bednet_obj$coverage
}

#' @title Get bednet coverage
#'
#' @description Get the values from a model
#' that set maximum bednet coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns maximum bednet coverage, as a vector
#'
#' @export
change_bednet_coverage = function(coverage, xds_obj){
  UseMethod("get_bednet_coverage", xds_obj$bednet_obj$cover_obj)
}

#' @title Set the cover_bednets
#'
#' @description Set the value of exogenous variables related to
#' cover_bednets
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Coverage <- function(t, y, xds_obj) {
  UseMethod("Bed_Net_Coverage", xds_obj$bednet_obj$cover_obj)
}

#' @title Set no cover_bednets
#'
#' @description The null model for cover_bednets
#'
#' @inheritParams Bed_Net_Coverage
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Coverage.none <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' Plot Bednet Coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_bednet_coverage = function(tt, xds_obj, clr="black", add=FALSE){
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- Bed_Net_Coverage(tt, y, xds_obj)
  coverage <- matrix(xds_obj$bednet_obj$coverage, byrow=T, ncol = xds_obj$nPatches)
  if(add==FALSE)
    graphics::plot(tt, 0*tt, type = "n", xlab="Time (Days)", ylab = "Coverage", ylim = range(0,coverage))
  for(i in 1:xds_obj$nPatches)
    graphics::lines(tt, coverage[,i], col=clr)
}
