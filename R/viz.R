
#' Plot coverage
#'
#' @param tt a set of time points
#' @param model a **`ramp.xds`** model object
#' @param mode a control mode
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** model object
#' @export
show_coverage = function(tt, model, mode, add=FALSE){
   class(mode) <- mode
   UseMethod("show_coverage", mode)
}

#' Plot IRS Coverage
#'
#' @inheritParams show_coverage
#'
#' @returns a **`ramp.xds`** model object
#' @export
show_coverage.irs = function(tt, model, mode, add=FALSE){
  model <- IRSCoverage(tt, model)
  plot(tt, model$irs$coverage, type = "l", xlab = "Time (Days)", ylab = "Coverage")
}

#' Plot Bed Net Coverage
#'
#' @inheritParams show_coverage
#'
#' @returns a **`ramp.xds`** model object
#' @export
show_coverage.bednet = function(tt, model, mode, add=FALSE){
  model <- bednetCoverage(tt, model)
  plot(tt, model$bednet$coverage, type = "l", xlab = "Time (Days)", ylab = "Coverage")
}
