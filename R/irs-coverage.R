
#' @title Set Up the IRS Coverage Object
#'
#' @description Set up an object to model
#' the effects of IRS, usually through the
#' computation of some intermediate variable
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_irs_coverage = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_irs_coverage", name)
}

#' @title Set Up the Null IRS Coverage Object
#'
#' @description
#' Set up a model for IRS effects that does nothing.
#'
#' @inheritParams setup_irs_coverage
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_coverage.none = function(name, xds_obj, options=list()){
  xds_obj$irs_obj$cover_obj = make_none_object()
  xds_obj$irs_obj$coverage = list()
  return(xds_obj)
}

#' @title Get irs coverage
#'
#' @description Get the values from a model
#' that set maximum irs coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
get_irs_coverage = function(xds_obj){
  xds_obj$irs_obj$coverage
}

#' @title Get irs coverage
#'
#' @description Get the values from a model
#' that set maximum irs coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns maximum irs coverage, as a vector
#'
#' @export
change_irs_coverage = function(coverage, xds_obj){
  UseMethod("get_irs_coverage", xds_obj$irs_obj$cover_obj)
}


#' Plot IRS Coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_irs_coverage = function(tt, xds_obj, clr="black", add=FALSE){
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- IRS_Coverage(tt, y, xds_obj)
  if(add==FALSE)
    graphics::plot(tt, xds_obj$irs_obj$coverage, type = "n", xlab="Time (Days)", ylab = "Coverage")
  graphics::lines(tt, xds_obj$irs_obj$coverage, col=clr)
}

#' @title IRS Coverage
#'
#' @description Compute IRS coverage
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
IRS_Coverage <- function(t, y, xds_obj) {
  UseMethod("IRS_Coverage", xds_obj$irs_obj$cover_obj)
}

#' @title Set no use_irss
#' @description The null model for use_irss
#' @inheritParams IRS_Coverage
#' @return [list]
#' @export
IRS_Coverage.none <- function(t, y, xds_obj) {
  return(xds_obj)
}
