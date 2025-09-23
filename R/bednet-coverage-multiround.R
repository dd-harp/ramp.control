# Bed Net multiround

#' @title Set Up a Bed Net Coverage Function
#'
#' @description
#' This sets up a function to set the value of bed net
#' coverage
#'
#' @inheritParams setup_bednet_coverage
#' @export
setup_bednet_coverage.multiround = function(name="func", xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$bednet_obj) = "dynamic"
  xds_obj$bednet_obj$cover_obj <- make_bednet_multiround(xds_obj, options)
  return(xds_obj)
}

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams Bed_Net_Coverage
#' @return a **`xds`** object
#' @export
Bed_Net_Coverage.multiround <- function(t, y, xds_obj) {
  with(xds_obj$bednet_obj$cover_obj,{
    xds_obj$bednet_obj$coverage = F_cover(t)
    return(xds_obj)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param options a list of options to override defaults
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param zap the contact parameter
#' @param type the BedNet type
#' @return a **`xds`** object
#' @export
make_bednet_multiround = function(options=list(),
                                t_init = 1,
                                coverage=.8,
                                zap=1,
                                type = "pbo"){
  with(options,{
    nRounds <- length(t_init)
    stopifnot(length(coverage) == nRounds)
    stopifnot(length(type) == nRounds)

    cover <- list()
    class(cover) <- "multiround"
    cover$nRounds = nRounds
    cover$t_init = t_init
    cover$coverage = coverage
    cover$type = type
    cover$zap = checkIt(zap, nRounds)

    cover = make_F_cover_bednet(cover)

    return(cover)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a list with parameters for all the rounds
#' @return a **`xds`** object
#' @export
make_F_cover_bednet = function(cover){

  rounds <- list()
  for(i in 1:cover$nRounds)
    rounds[[i]] = with(cover, make_bednet_round(type[i], t_init[i], coverage[i], zap[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  cover$F_cover <- make_function(rounds_par)

  return(cover)
}


#' Set values for bednet coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
change_bednet_coverage.multiround = function(coverage, xds_obj){
  xds_obj$bednet$cover_obj$coverage = coverage
  return(xds_obj)
}


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param xds_obj the `ramp.xds` object
#' @param type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param zap the coverage achieved
#' @return a **`xds`** object
#' @export
add_bednet_round = function(xds_obj, type, t_init, coverage, zap=1) {
  options <- list()
  options$type = c(xds_obj$bednet$cover_obj$type, type)
  options$t_init = c(xds_obj$bednet$cover_obj$t_init, t_init)
  options$coverage = c(xds_obj$bednet$cover_obj$coverage, coverage)
  options$zap = c(xds_obj$bednet$cover_obj$zap, zap)
  xds_obj$bednets$cover_obj = make_bednet_multiround(options)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param type the name of the BedNet type
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param zap contact scaling
#' @return a **`xds`** object
#' @export
make_bednet_round = function(type, t_init, coverage, zap=1) {
  class(type) <- type
  UseMethod("make_bednet_round", type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when BedNet started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param coverage the coverage achieved
#' @param zap contact scaling parameter
#' @return a **`xds`** object
#' @export
make_bednet_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, coverage=.7, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=coverage^zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_bednet_round
#' @return a **`xds`** object
#' @export
make_bednet_round.pbo = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage^zap)
}
