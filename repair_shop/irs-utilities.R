
#' @title Get irs coverage
#'
#' @description Get the values from a model
#' that set maximum irs coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns IRS coverage
#'
#' @export
get_irs_coverage = function(xds_obj){
  UseMethod("get_irs_coverage", xds_obj$irs$coverage_mod)
}

#' @title Get irs coverage
#'
#' @description Get the values from a model
#' that set maximum irs coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#' @param coverage coverage parameters
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_irs_coverage = function(coverage, xds_obj){
  UseMethod("get_irs_coverage", xds_obj$irs$coverage_mod)
}

#' Set values for irs coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns IRS coverage
#'
#' @export
get_irs_coverage.multiround = function(xds_obj){
  return(xds_obj$irs$coverage_mod$coverage)
}

#' Set values for irs coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
set_irs_coverage.multiround = function(coverage, xds_obj){
  xds_obj$irs$coverage_mod$coverage = coverage
  return(xds_obj)
}

#' Plot coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_irs_coverage = function(tt, xds_obj, clr="black", add=FALSE){
  xds_obj <- IRSCoverage(tt, xds_obj)
  if(add==FALSE) graphics::plot(tt, xds_obj$irs$coverage, type = "l", col=clr, xlab = "Time (Days)", ylab = "Coverage")
  if(add==TRUE) graphics::lines(tt, xds_obj$irs$coverage, col=clr)
}
