#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a list with parameters for all the rounds
#' @return a function
#' @export
make_F_cover_bednet = function(cover){

  rounds <- list()
  for(i in 1:cover$nRounds)
    rounds[[i]] = with(cover, make_bednet_round(type[i], t_init[i], peak[i], contact[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  F_cover <- make_function(rounds_par)

  return(F_cover)
}

#' @title Make the Multiround Bednet Coverage Object
#'
#' @description
#'
#' Using information about the events (see [setup_bednet_events]),
#' including a parameter describing access, this makes a
#' function that computes coverage over time.
#'
#'
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param use_contact TRUE uses contact parameters from the events object
#'
#' @return a **`xds`** object
#' @export
make_bednet_multiround = function(xds_obj, use_contact=FALSE){
  with(xds_obj$events_obj, stopifnot(exists("bednet")))
  with(xds_obj$events_obj$bednet,{
    stopifnot(N>0)
    if(!exists("include")) include = rep(TRUE, N)
    nRounds <- sum(include)
    cover <- list()
    class(cover) <- "multiround"
    cover$nRounds = nRounds
    cover$t_init  = jdate[include]
    cover$peak    = peak[include]
    cover$type    = type[include]

    if(use_contact==TRUE){
      cover$contact = contact[include]
      cover$F_contact <- make_F_cover_bednet(cover)
    } else {
      cover$contact = rep(1, nRounds)
      cover$F_cover <- make_F_cover_bednet(cover)
    }

    return(cover)
})}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param peak the peack access achieved
#' @param contact contact scaling
#'
#' @return a **`xds`** object
#' @export
make_bednet_round = function(type, t_init, peak, contact=1) {
  class(type) <- type
  UseMethod("make_bednet_round", type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @param t_init the time when BedNet started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param peak the peak access achieved
#' @param contact contact scaling parameter
#'
#' @return a **`xds`** object
#' @export
make_bednet_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, peak=.7, contact=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=peak*contact)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_bednet_round
#' @return a **`xds`** object
#' @export
make_bednet_round.pbo = function(type, t_init, peak, contact=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=peak*contact)
}
