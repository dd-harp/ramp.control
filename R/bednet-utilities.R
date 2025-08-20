
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
  UseMethod("get_bednet_coverage", xds_obj$bednet$coverage_mod)
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
set_bednet_coverage = function(coverage, xds_obj){
  UseMethod("get_bednet_coverage", xds_obj$bednet$coverage_mod)
}

#' Set values for bednet coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns maximum bednet coverage, as a vector
#'
#' @export
get_bednet_coverage.multiround = function(xds_obj){
  coverage <- xds_obj$bednet$coverage_mod$coverage
  return(coverage)
}

#' Set values for bednet coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_bednet_coverage.multiround = function(coverage, xds_obj){
  xds_obj$bednet$coverage_mod$coverage = coverage
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
  xds_obj <- BedNetCoverage(tt, xds_obj)
  if(add==FALSE) graphics::plot(tt, xds_obj$bednet$coverage, type = "l", col=clr, xlab = "Time (Days)", ylab = "Coverage")
  if(add==TRUE) graphics::lines(tt, xds_obj$bednet$coverage, col=clr)
}
