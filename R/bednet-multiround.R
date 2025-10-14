#' @title Set up dynamic forcing
#'
#' @description Make a multiround function
#'
#' @param multi_obj a list with parameters for all the rounds
#'
#' @return a function
#' @export
make_F_multi_bednet = function(multi_obj){

  rounds <- list()
  for(i in 1:multi_obj$N)
    rounds[[i]] = with(multi_obj, make_bednet_round(type[i], jdate[i], peak[i], contact[i]))

  rounds_par <- makepar_F_multiround(multi_obj$N, rounds)
  multi_obj$rounds <- rounds
  multi_obj$rounds_par <- rounds_par
  F_multi <- make_function(rounds_par)

  return(F_multi)
}

#' @title Make the Multiround Bednet Coverage Object
#'
#' @description
#'
#' Using information about the events (see [setup_bednet_events]),
#' including a parameter describing access, this makes a
#' function that computes coverage over time.
#'
#' @param contact a contact parameter
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`xds`** object
#' @export
make_bednet_multiround = function(contact, xds_obj){
  with(xds_obj$events_obj, stopifnot(exists("bednet")))
  bednet <- xds_obj$events_obj$bednet
  bednet$contact <- contact
  make_F_multi_bednet(bednet)
}


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
