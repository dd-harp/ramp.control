
#' Setup IRS Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param start_day the Julian start dates of IRS events
#' @param pesticides the pesticide used
#' @param frac_sprayed the fraction of houses sprayed (if known)
#' @param event_length the number of days it took to spray the houses
#' @param contact maximum effective contact
#' @param shock maximum shock size
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_irs_events = function(xds_obj, start_day, pesticides, frac_sprayed=0.5, event_length=20, contact=.5, shock=.5){

  xds_obj <- setup_vector_control(xds_obj)

  if(with(xds_obj,!exists("events_obj")))
    xds_obj$events_obj = list()

  N = length(start_day)
  stopifnot(length(pesticides)==N)
  frac_sprayed = checkIt(frac_sprayed, N)
  elength = checkIt(event_length, N)

  irs = list()
  irs$N = N
  irs$start_day = start_day
  irs$type = pesticides
  irs$coverage = frac_sprayed
  irs$event_length = checkIt(elength, N)
  irs$contact = checkIt(contact, N)
  irs$shock = checkIt(shock, N)
  irs$pw = rep(1,N)

  irs$D = start_day+elength/2
  irs$uk = 10/elength
  irs$d_50 = rep(0, N)
  irs$d_shape = rep(0, N)

  for(i in 1:N){
    profile = irs_profiles[irs_profiles$name == pesticides[i],]
    irs$d_50[i] = profile$d_50
    irs$d_shape[i] = 1/profile$d_shape
  }

  xds_obj$events_obj$irs = irs

  return(xds_obj)
}

#' @title Add IRS rounds
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @inheritParams setup_irs_events
#'
#' @return a **`xds`** object
#' @export
add_irs_events = function(xds_obj, start_day, pesticides, frac_sprayed, event_length=20, contact=1, shock=1){
  M = length(start_day)
  stopifnot(length(pesticides)==M)
  stopifnot(length(frac_sprayed)==M)
  event_length= checkIt(event_length, M)
  coverage = checkIt(coverage, M)
  contact = checkIt(contact, M)
  shock = checkIt(shock, M)

  if(with(xds_obj$events_obj, !exists("irs")))
    return(setup_irs_events(xds_obj, start_day, pesticides, frac_sprayed, event_length, contact, shock))

  N = xds_obj$events_obj$irs$N

  irs = xds_obj$events_obj$irs
  irs$start_day = c(irs$start_day, start_day)
  irs$type = c(irs$type, pesticides)
  irs$event_length = c(irs$event_length, event_length)
  irs$coverage = c(irs$coverage, frac_sprayed)
  irs$contact = c(irs$contact, contact)
  irs$shock = c(irs$shock, shock)
  irs$pw = c(irs$pw, rep(1,M))

  irs$D = c(irs$D, start_day+event_length/2)
  irs$uk = c(irs$uk, 10/event_length)
  d_50 = rep(0, M)
  d_shape = rep(0, M)

  for(i in 1:M){
    profile = irs_profiles[irs_profiles$name == pesticides[i],]
    d_50[i] = profile$d_50
    d_shape[i] = 1/profile$d_shape
  }
  irs$d_50 = c(irs$d_50, d_50)
  irs$d_shape = c(irs$d_shape, d_shape)

  xds_obj$events_obj$irs = irs

  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description Set up the IRS rounds
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mx the maximum
#' @param as_shock set the function class to `sharkfin` (if `FALSE`) or `sharkbite` (if `TRUE`)
#'
#' @return set up the rounds
#'
#' @export
setup_irs_rounds = function(xds_obj, mx, as_shock=FALSE){
  xds_obj$events_obj$irs$rounds = list()
  with(xds_obj$events_obj$irs,{
    stopifnot(length(mx)==N)
    if(N>0)
    for(i in 1:N){
      irs_round = list()
      class(irs_round) = ifelse(as_shock, "sharkbite", "sharkfin")
      irs_round$D = start_day[i]+event_length[i]/2
      irs_round$L = d_50[i]
      irs_round$uk = 10/event_length[i]
      irs_round$dk = d_shape[i]
      irs_round$pw = pw[i]
      irs_round$mx = mx[i]
      irs_round$N = xds_obj$nPatches
      xds_obj$events_obj$irs$rounds[[i]] = irs_round
    }
  return(xds_obj)
})}
