
#' @title Setup Bed Net Shocks
#'
#' @description Set up a function that computes
#' bednet multi_objage over time.
#'
#' @param xds_obj an **`xds`**  model object
#' @param options a list of options
#'
#' @return an **`xds`** model object
#'
#' @export
setup_bednet_shock_multiround = function(xds_obj, options=list()){
  N = xds_obj$events_obj$bednet$N

  shock = rep(0, N)
  shock = with(options, shock)
  stopifnot(length(shock)==N)
  xds_obj$events_obj$bednet$shock = shock

  pw = rep(1, N)
  pw = with(options, pw)
  stopifnot(length(pw)==N)
  xds_obj$events_obj$bednet$pw = pw

  xds_obj <- setup_F_multishock(xds_obj)
  return(xds_obj)
}

#' @title Change bednet Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of bednet on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param shock the scaling parameter
#'
#' @return an **`xds`** model object
#'
#' @export
change_bednet_shock_multiround = function(xds_obj, shock){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))

  stopifnot(length(shock) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$shock = shock

  xds_obj = setup_F_multishock(xds_obj)

  return(xds_obj)
}

#' Plot bednet Coverage
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_bednet_shock = function(tt, xds_obj, clr="black", add=FALSE){
  if(add==FALSE)
    graphics::plot(tt, xds_obj$EIR_obj$F_shock(tt), type = "n", xlab="Time (Days)", ylab = "Coverage")
  graphics::lines(tt, xds_obj$EIR_obj$F_shock(tt), col=clr)
}

