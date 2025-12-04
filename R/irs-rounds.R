
#' @title Make an IRS Round
#'
#' @description Return the parameters
#' to make sharkfin function for an irs round with one of the
#' pesticides in the `irs_profiles` table.
#'
#' The parameters
#' specify a model for the
#' "killing potential" from the start of the spray round through the end.
#'
#' @param irs_type the name of the IRS type
#' @param start_day the start day for the IRS round
#' @param peak the maximum value
#' @param elength the number of days it took to complete spraying
#' @param pw a shape parameter
#'
#' @return a `sharkfin` function object
#'
#' @export
make_irs_round = function(irs_type, start_day, peak, elength=20, pw=1) {
  profile = irs_profiles[irs_profiles$name == irs_type,]
  D = start_day+elength/2
  uk = 10/elength
  L = profile$d_50
  dk = 1/profile$d_shape
  return(makepar_F_sharkfin(D=D, L=L, uk=uk,dk=dk, mx=peak, pw=pw))
}

#' @title Make an IRS Round
#'
#' @description Return the parameters
#' to make sharkfin function for an irs round with one of the
#' pesticides in the `irs_profiles` table.
#'
#' The parameters
#' specify a model for the
#' "killing potential" from the start of the spray round through the end.
#'
#' @inheritParams make_irs_round
#'
#' @return a `sharkbite` function object
#'
#' @export
make_irs_shock = function(irs_type, start_day, peak, elength=20, pw=1) {
  profile = irs_profiles[irs_profiles$name == irs_type,]
  D = start_day+elength/2
  uk = 10/elength
  L = profile$d_50
  dk = 1/profile$d_shape
  return(makepar_F_sharkbite(D=D, L=L, uk=uk,dk=dk, mx=peak,pw=pw))
}

#' @title Make an IRS Killing Profile
#'
#' @description Return the parameters
#' to make sharkfin function for an irs round.
#'
#'
#' @param d_50 the day when efficacy reaches 50%
#' @param d_shape the decay shape
#' @param start_day the start day for the IRS round
#' @param peak the maximum value
#' @param elength the number of days it took to complete spraying
#' @param pw a shape parameter
#'
#' @return a `sharkfin` function object
#'
#' @export
make_irs_killing_profile = function(d_50, d_shape, start_day, peak, elength=20, pw=1) {
  D = start_day+elength/2
  uk = 5/elength
  return(makepar_F_sharkfin(D=D, uk=uk, L=d_50, dk=1/d_shape, mx=peak, pw=pw))
}

#' @title Make an IRS Effect Size Curve
#'
#' @description Return the parameters
#' to make sharkfin function for an irs round.
#'
#' @inheritParams make_irs_killing_profile
#'
#' @return a `sharkbite` function object
#'
#' @export
make_irs_efsz_profile = function(d_50, d_shape, start_day, peak, elength=20, pw=1) {
  D = start_day+elength/2
  uk = 5/elength
  return(makepar_F_sharkbite(D=D, uk=uk, L=d_50, dk=1/d_shape, mx=peak))
}


#' @title Show an IRS Profile
#'
#' @description Return the parameters
#' to call a sharkfin function to model
#' a single round of IRS.
#'
#' @param irs_type the name of the IRS type
#'
#' @return a **`xds`** object
#' @export
show_irs_profile = function(irs_type){
  pars <- make_irs_round(irs_type, 10, 1)
  mtl <- paste("IRS Killing Potential (", irs_type, ")", sep="")
  ylb <- "Pr(Death)"
  F_kill <- make_function(pars)
  tt <- c(0:730)
  plot(tt, F_kill(tt), main=mtl, ylab=ylb, type = "l")
}

#' @title Make Parameters for an IRS Round
#'
#' @description Return the parameters
#' to call a sharkfin function to model
#' a single round of IRS.
#'
#' @param irs_type the name of the IRS type
#'
#' @return a **`xds`** object
#' @export
show_irs_response_timeline = function(irs_type){
  pars <- make_irs_shock(irs_type, 10, 1)
  mtl <- paste("IRS - EIR Response Timeline (", irs_type, ")", sep="")
  ylb <- "Relative Effect"
  F_kill <- make_function(pars)
  tt <- c(0:730)
  plot(tt, F_kill(tt), main=mtl, ylab=ylb, type = "l")
}
