
#' @title Make an Bed Net Round
#'
#' @description Return the parameters
#' to make sharkfin function for a bed net round with one of the
#' effective coverage profiles in the `bednet_profiles` table.
#'
#' The parameters
#' specify a model for the
#' "effective coverage" after a mass bed net distribution
#'
#' @param d_50 the half-saturation day
#' @param d_shape the shape
#' @param start_day the start day for the bednet round
#' @param peak the maximum value
#' @param length the number of days it took to complete spraying
#' @param pw a shape parameter
#'
#' @return a `sharkfin` function object
#'
#' @export
make_bednet_round = function(d_50, d_shape, start_day, peak, length=20, pw=1) {
  D = start_day+length/2
  uk = 10/length
  L = d_50
  dk = d_shape
  return(makepar_F_sharkfin(D=D, L=L, uk=uk, dk=dk, mx=peak, pw=pw))
}

#' @title Make a Bed Net Round
#'
#' @description Return the parameters
#' to make sharkfin function for an bednet round with one of the
#' pre-defined profiles in the `bednet_profiles` table.
#'
#' The parameters
#' specify a model for bed net coverage
#' after a mass distribution
#'
#' @inheritParams make_bednet_round
#'
#' @return a `sharkbite` function object
#'
#' @export
make_bednet_shock = function(d_50, d_shape, start_day, peak, length=20, pw=1) {
  D = start_day+length/2
  uk = 10/length
  L = d_50
  dk = d_shape
  return(makepar_F_sharkbite(D=D, L=d_50, uk=uk, dk=d_shape, mx=peak, pw=pw))
}

#' @title Make an Bed Net Coverage Profile
#'
#' @description Return the parameters
#' to make sharkfin function for an bednet round.
#'
#'
#' @param d_50 the day when efficacy reaches 50%
#' @param d_shape the decay shape
#' @param start_day the start day for the bednet round
#' @param peak the maximum value
#' @param length the number of days it took to complete spraying
#' @param pw a shape parameter
#'
#' @return a `sharkfin` function object
#'
#' @export
make_bednet_profile = function(d_50, d_shape, start_day, peak, length=20, pw=1) {
  D = start_day+length/2
  uk = 5/length
  return(makepar_F_sharkfin(D=D, uk=uk, L=d_50, dk=1/d_shape, mx=peak, pw=pw))
}

#' @title Make an bednet Effect Size Curve
#'
#' @description Return the parameters
#' to make sharkfin function for an bednet round.
#'
#' @inheritParams make_bednet_profile
#'
#' @return a `sharkbite` function object
#'
#' @export
make_bednet_efsz_profile = function(d_50, d_shape, start_day, peak, length=20, pw=1) {
  D = start_day+length/2
  uk = 5/length
  return(makepar_F_sharkbite(D=D, uk=uk, L=d_50, dk=1/d_shape, mx=peak))
}

#' @title Show an bednet Profile
#'
#' @description Return the parameters
#' to call a sharkfin function to model
#' a single round of bednet.
#'
#' @param d_50 the half-saturation parameter
#' @param d_shape the shape parameter
#' @param clr the line color
#' @param add if TRUE, add to an existing plot
#'
#' @return a **`xds`** object
#' @export
show_bednet_profile = function(d_50, d_shape, clr="black", add=FALSE){
  pars <- make_bednet_round(d_50, d_shape, 10, 1)
  ylb <- "Effective Coverage"
  F_kill <- make_function(pars)
  tt <- c(0:(4*pars$L))
  if(add==FALSE) plot(tt, F_kill(tt), ylab=ylb, type = "n")
  lines(tt, F_kill(tt), col = clr)
}

#' @title Make Parameters for an bednet Round
#'
#' @description Return the parameters
#' to call a sharkfin function to model
#' a single round of bednet.
#'
#' @param d_50 the half-saturation parameter
#' @param d_shape the shape parameter
#' @param clr the line color
#' @param add if TRUE, add to an existing plot
#'
#' @return a **`xds`** object
#' @export
show_bednet_response_timeline = function(d_50, d_shape, clr="black", add=FALSE){
  pars <- make_bednet_shock(d_50, d_shape, 10, 1)
  ylb <- "Relative Effect"
  F_kill <- make_function(pars)
  tt <- c(0:(4*pars$L))
  if(add==FALSE) plot(tt, F_kill(tt), ylab=ylb, type = "n")
  lines(tt, F_kill(tt), col = clr)
}



