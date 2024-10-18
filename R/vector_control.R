
#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, pars) {
  pars = BedNet(t, pars)
  pars = IRS(t, pars)
  #  pars = AreaSpray(t, pars)
  #  pars = SugarBait(t, pars)
  #  pars = LSM(t, pars)
  #  pars = EM(t, pars)
  #  pars = Endectocide(t, pars)
  #  pars = ADLarvicide(t, pars)
  return(pars)
}

#' @title The `setup` case for exogenous vector_control
#' @description Call all the functions to set the
#' values of exogenous variables and then revert
#' the `none` case
#' @param t current simulation time
#' @param pars an **`xds`** object
#' @return an **`xds`** object
VectorControl.setup = function(t, pars){
  class(pars$vector_control) <- 'dynamic'
  pars <- VectorControl(t, pars)
  class(pars$vector_control) <- 'none'
  return(pars)
}


#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.dynamic <- function(t, y, pars) {
  for(s in 1:pars$nVectors){
    pars <- BedNetEffectSizes(t, pars, s)
    pars <- IRSEffectSizes(t, pars, s)
  }
  #  pars = AreaSprayEffectSizes(t, pars)
  #  pars = SugarBaitEffectSizes(t, pars)
  #  pars = LSMEffectSizes(t, pars)
  #  pars = EM_EffectSizes(t, pars)
  #  pars = Endectocide_EffectSizes(t, pars)
  #  pars = ADLarvicide_EffectSizes(t, pars)
  return(pars)
}
