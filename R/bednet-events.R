
#' Setup bednet Events
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param start_day the Julian start dates of bednet events
#' @param type the type of net used
#' @param event_length the number of days it took to spray the houses
#' @param coverage maximum effective coverage
#' @param contact maximum effective contact
#' @param shock maximum shock size
#' @param d_50 half-coverage parameter
#' @param d_shape coverage shape parameter
#'
#' @returns a **`ramp.xds`**  model object
#'
#' @export
setup_bednet_events = function(xds_obj, start_day, type = "pbo", event_length=20, coverage=1, contact=1, shock=0.5, d_50=365, d_shape=1/365){

  xds_obj <- setup_vector_control(xds_obj)

  if(with(xds_obj,!exists("events_obj")))
    xds_obj$events_obj = list()

  N = length(start_day)

  elength=checkIt(event_length, N)

  bednet = list()
  bednet$N = N
  bednet$start_day = start_day

  if(length(type)==1) type=rep(type, N)
  stopifnot(length(type)==N)
  bednet$type = type

  bednet$event_length = checkIt(elength, N)
  bednet$coverage = checkIt(coverage, N)
  bednet$contact = checkIt(contact, N)
  bednet$shock = checkIt(shock, N)
  bednet$d_50 = checkIt(d_50, N)
  bednet$d_shape = checkIt(d_shape, N)
  bednet$pw = rep(1,N)

  xds_obj$events_obj$bednet = bednet

  return(xds_obj)
}

#' @title Add bednet rounds
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @inheritParams setup_bednet_events
#'
#' @return a **`xds`** object
#' @export
add_bednet_events = function(xds_obj, start_day, type = "pbo", event_length=20, coverage=1, contact=1, shock=1, d_50 = 365, d_shape=1/365){
  M = length(start_day)
  if(length(type)==1) type = rep(type, M)
  stopifnot(length(type) == M)
  event_length= checkIt(event_length, M)
  coverage = checkIt(coverage, M)
  contact = checkIt(contact, M)
  shock = checkIt(shock, M)
  bednet$d_50 = checkIt(d_50, M)
  bednet$d_shape = checkIt(d_shape, M)
  bednet$pw = rep(1,M)

  if(with(xds_obj$events_obj, !exists("bednet")))
    return(setup_bednet_events(xds_obj, start_day, type, event_length, coverage, contact, shock, d_50, d_shape))

  N = xds_obj$events_obj$bednet$N

  bednet = xds_obj$events_obj$bednet
  bednet$start_day = c(bednet$start_day, start_day)
  bednet$type = c(bednet$type, type)
  bednet$event_length = c(bednet$event_length, event_length)
  bednet$coverage = c(bednet$coverage, coverage)
  bednet$contact = c(bednet$contact, contact)
  bednet$shock = c(bednet$shock, shock)
  bednet$d_50 = c(bednet$d_50, d_50)
  bednet$d_shape = c(bednet$d_shape, d_shape)
  bednet$pw = c(bednet$pw, rep(1,M))

  xds_obj$events_obj$bednet = bednet

  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description Set up the bednet rounds
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param mx the maximum
#' @param as_shock set the function class to `sharkfin` (if `FALSE`) or `sharkbite` (if `TRUE`)
#'
#' @return set up the rounds
#'
#' @export
setup_bednet_rounds = function(xds_obj, mx, as_shock=FALSE){
  xds_obj$events_obj$bednet$rounds = list()
  with(xds_obj$events_obj$bednet,{
    stopifnot(length(mx)==N)
    for(i in 1:N){
      round = list()
      class(round) = ifelse(as_shock, "sharkbite", "sharkfin")
      round$D = start_day[i]+event_length[i]/2
      round$L = d_50[i]
      round$uk = 10/event_length[i]
      round$dk = d_shape[i]
      round$pw = pw[i]
      round$mx = mx[i]
      round$N = xds_obj$nPatches
      xds_obj$events_obj$bednet$rounds[[i]] = round
  }
  return(xds_obj)
})}

