
#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param type the name of the IRS type
#' @param t_init the time when IRS started
#' @param coverage the coverage achieved
#' @param zap contact scaling
#' @return a **`xds`** object
#' @export
make_irs_round = function(type, t_init, coverage, zap=1) {
  class(type) <- type
  UseMethod("make_irs_round", type)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param t_init the time when IRS started
#' @param uk scale up rate
#' @param L length of half-life
#' @param dk decay shape parameter
#' @param coverage the coverage achieved
#' @param zap contact scaling parameter
#' @return a **`xds`** object
#' @export
make_irs_round_generic = function(t_init, uk=1/5, L=365, dk=1/60, coverage=.7, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=uk, L=L, dk = dk, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.none = function(type, t_init, coverage, zap=1) {
  makepar_F_zero()
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.actellic = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/60, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.bendiocarb = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=100, dk = 1/25, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.fludora_fusion = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=310, dk = 1/35, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.sumishield = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=365, dk = 1/75, mx=coverage)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams make_irs_round
#' @return a **`xds`** object
#' @export
make_irs_round.pyrethroid = function(type, t_init, coverage, zap=1) {
  makepar_F_sharkfin(D=t_init, uk=1/5, L=180, dk = 1/100, mx=coverage)
}
