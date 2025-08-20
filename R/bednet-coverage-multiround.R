# Bed Net multiround

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams BedNetCoverage
#' @return a **`xds`** object
#' @export
BedNetCoverage.multiround <- function(t, xds_obj) {with(xds_obj$bednet$coverage_mod,{
  xds_obj$bednets$coverage = F_cover(t)
  return(xds_obj)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_coverage
#' @return a **`xds`** object
#' @export
setup_bednet_coverage.multiround = function(name, xds_obj, opts=list()){
  setup_bednet_multiround(opts)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param opts a list of options to override defaults
#' @param t_init the time when BedNet started
#' @param coverage the coverage achieved
#' @param type the BedNet type
#' @param zap the contact parameter
#' @return a **`xds`** object
#' @export
setup_bednet_multiround = function(opts=list(),
                                t_init = 1,
                                coverage=.8,
                                type = "pbo",
                                zap = 1){
  with(opts,{
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

    cover = setup_F_cover_bednet(cover)

    return(cover)
})}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param cover a list with parameters for all the rounds
#' @return a **`xds`** object
#' @export
setup_F_cover_bednet = function(cover){

  rounds <- list()
  for(i in 1:cover$nRounds)
    rounds[[i]] = with(cover, setup_bednet_round(type[i], t_init[i], coverage[i], zap[i]))

  rounds_par <- makepar_F_multiround(cover$nRounds, rounds)
  cover$rounds <- rounds
  cover$rounds_par <- rounds_par
  cover$F_cover <- make_function(rounds_par)

  return(cover)
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
  opts <- list()
  opts$type = c(xds_obj$bednet$coverage_mod$type, type)
  opts$t_init = c(xds_obj$bednet$coverage_mod$t_init, t_init)
  opts$coverage = c(xds_obj$bednet$coverage_mod$coverage, coverage)
  opts$zap = c(xds_obj$bednet$coverage_mod$zap, zap)
  xds_obj$bednets$coverage_mod = setup_bednet_multiround(opts)
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
setup_bednet_round = function(type, t_init, coverage, zap=1) {
  class(type) <- type
  UseMethod("setup_bednet_round", type)
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
setup_bednet_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, coverage=.7, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=coverage^zap)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_bednet_round
#' @return a **`xds`** object
#' @export
setup_bednet_round.pbo = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage^zap)
}

