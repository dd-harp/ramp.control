
#' @title Change IRS Shocks
#'
#' @description
#' For `eir` models, set up a multi-round
#' sharkbite function to model effect sizes
#' of IRS on the EIR as a perturbation.
#'
#' @param xds_obj an **`xds`** model object
#' @param shock the scaling parameter
#'
#' @export
change_irs_shock_multiround = function(xds_obj, shock){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))

  stopifnot(length(shock) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$shock = shock

  xds_obj <- setup_irs_rounds(xds_obj, shock, TRUE)
  xds_obj = setup_F_multishock(xds_obj)
  return(xds_obj)
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
show_irs_shock = function(tt, xds_obj, clr="black", add=FALSE){
  xds_obj <- setup_F_multishock(xds_obj)
  if(add==FALSE)
    graphics::plot(tt, xds_obj$EIR_obj$F_shock(tt), type = "n", xlab="Time (Days)", ylab = "Coverage")
  graphics::lines(tt, xds_obj$EIR_obj$F_shock(tt), col=clr)
}

