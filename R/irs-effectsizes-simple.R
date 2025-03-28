
#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @inheritParams setup_irs_effectsizes
#' @export
setup_irs_effectsizes.simple = function(name, pars, s=1, opts=list()){
  pars = setup_irs_effectsizes_simple(pars, s, opts)
  return(pars)
}

#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param pars an **`xds`** object
#' @param s the vector species index
#' @param opts a list of options to override defaults
#' @param contact the probability of contact given coverage
#' @return an **`xds`** object
#' @export
setup_irs_effectsizes_simple = function(pars, s=1, opts=list(),
                                        contact=1){
  es <- list()
  es$name <- 'simple'
  class(es) <- 'simple'
  es$contact <- contact
  pars$irs$effectsizes <-  list()
  pars$irs$effectsizes[[s]] <- es
  return(pars)
}

#' @title Modify baseline values due to vector control
#' @description Implements [IRSEffectSizes] for the Le Menach IRS model of vector control
#' @inheritParams IRSEffectSizes
#' @return a named [list]
#' @importFrom stats pexp
#' @seealso [compute_irs_effect_sizes_simple()]
#' @export
IRSEffectSizes.simple <- function(t, pars, s){
  phi = pars$vars$irs_coverage
  with(pars$irs$effectsizes[[s]],{
    with(pars$MYZpar[[s]],{
      es <- sapply(1:pars$nPatches,
                   compute_irs_effect_sizes_simple,
                   phi=phi, ff=f_t, qq=q_t, gg=g_t, contact)
      pars$MYZpar[[s]]$es_g <- pars$MYZpar[[s]]$es_g*es
      return(pars)
    })
})}


#' @title Modify baseline values due to vector control
#' @description Implements a model published in 2007
#' @param ix an index over nPatches
#' @param phi IRS coverage
#' @param ff baseline blood feeding rate
#' @param qq baseline human fraction
#' @param gg baseline mosquito mortality rate
#' @param contact the probability of contact given coverage
#' @return an **`xds`** model object
#' @importFrom stats pexp
compute_irs_effect_sizes_simple = function(ix, phi, ff, qq, gg, contact=0.8){
  f=ff[ix]; q=qq[ix]; g=gg[ix]
  es = (gg+ff*qq*phi*contact)/gg
  return(c(es))
}

