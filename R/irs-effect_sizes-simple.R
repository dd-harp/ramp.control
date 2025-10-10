
#' @title Set up dynamic forcing
#'
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#'
#' @inheritParams setup_irs_effect_sizes
#'
#' @export
setup_irs_effect_sizes.simple = function(name, xds_obj, s=1, options=list()){
  xds_obj$irs_obj$eff_sz_obj = make_irs_effect_sizes_simple(options)
  return(xds_obj)
}

#' @title Set up dynamic forcing
#'
#' @description
#' A simple model for IRS effect sizes
#'
#' @param options a list of options to override defaults
#' @param contact the probability of contact given coverage
#'
#' @return a **`ramp.xds`** model object
#' @export
make_irs_effect_sizes_simple = function(options=list(),
                                        contact=1){
  es <- list()
  es$name <- 'simple'
  class(es) <- 'simple'
  return(es)
}

#' @title Modify baseline values due to vector control
#' @description Implements [IRS_Effect_Sizes] for the Le Menach IRS model of vector control
#' @inheritParams IRS_Effect_Sizes
#' @return a named [list]
#' @importFrom stats pexp
#' @seealso [compute_irs_effect_sizes_simple()]
#' @export
IRS_Effect_Sizes.simple <- function(t, y, xds_obj, s){
  contact = xds_obj$irs_obj$contact
  with(xds_obj$MY_obj[[s]],{
    es <- sapply(1:xds_obj$nPatches,
                 compute_irs_effect_sizes_simple,
                 ff=f_t, qq=q_t, gg=g_t, contact=contact)
    xds_obj$MY_obj[[s]]$es_g <- xds_obj$MY_obj[[s]]$es_g*es
    return(xds_obj)
})}


#' @title Modify baseline values due to vector control
#' @description Implements a model published in 2007
#' @param ix an index over nPatches
#' @param ff baseline blood feeding rate
#' @param qq baseline human fraction
#' @param gg baseline mosquito mortality rate
#' @param contact the probability of contact given coverage
#' @return a **`ramp.xds`** model object
#' @importFrom stats pexp
compute_irs_effect_sizes_simple = function(ix, ff, qq, gg, contact){
  f=ff[ix]; q=qq[ix]; g=gg[ix]; contact=contact[ix]
  es = (gg+ff*qq*contact)/gg
  return(c(es))
}

