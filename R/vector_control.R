
#' @title Implement Vector Control
#' @description Implements various forms
#' of vector control. Each mode for vector
#' control is set up and configured separately.
#' @note This a junction to implement various modes of
#' vector control.
#' Non-trivial vector control modules are in
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' @inheritParams ramp.xds::VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, pars) {
  pars = BedNet(t, pars)
  pars = IRS(t, pars)
  pars = AreaSpray(t, pars)
  pars = SugarBaits(t, pars)
  pars = LSM(t, pars)
  #  pars = EM(t, pars)
  #  pars = Endectocide(t, pars)
  #  pars = ADLarvicide(t, pars)
  return(pars)
}

#' @title Vector Control for Static Vector Control
#' @description The `setup` case runs the `dynamic`
#' case once, to set the values of variables for
#' static models. It then reverts to `none` so that
#' those values are not changed again.
#' @inheritParams ramp.xds::VectorControl
#' @return an **`xds`** object
#' @export
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

  pars <- BedNetCoverage(t, pars)
  pars <- IRSCoverage(t, pars)
  pars <- AreaSprayCoverage(t, pars)
  pars <- SugarBaitCoverage(t, pars)
  pars <- LSMCoverage(t, pars)

  for(s in 1:pars$nVectors){
    pars <- BedNetEffectSizes(t, pars, s)
    pars <- IRSEffectSizes(t, pars, s)
    pars <- AreaSprayEffectSizes(t, pars, s)
    pars <- SugarBaitEffectSizes(t, pars, s)
    pars <- LSMEffectSizes(t, pars, s)
  }
  #  pars = EM_EffectSizes(t, pars)
  #  pars = Endectocide_EffectSizes(t, pars)
  #  pars = ADLarvicide_EffectSizes(t, pars)
  return(pars)
}

#' @title Turn On Vector Control
#' @description Any function that sets up
#' non-trivial vector control must
#' calls this function.
#' If `class(pars$vector_control) == 'none'`
#' then it is set to `dynamic` and the
#' trivial module for every mode of
#' vector control is set up. Otherwise,
#' nothing happens.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control = function(pars){
  UseMethod("dynamic_vector_control", pars$vector_control)
}

#' @title Turn On Vector Control
#' @description If
#' `class(pars$vector_control) == 'none'`
#' then dynamic vector_control has not
#' been set up by any other function.
#' This sets `class(vector_control) <- 'dynamic'` and
#' then sets up a  trivial module for every mode of
#' vector control.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.none = function(pars){
  vector_control <- 'dynamic'
  class(vector_control) <- 'dynamic'
  pars$vector_control <- vector_control
  pars <- setup_no_bednets(pars)
  pars <- setup_no_irs(pars)
  pars <- setup_no_area_spray(pars)
  pars <- setup_no_lsm(pars)
  pars <- setup_no_sugar_baits(pars)
  return(pars)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(pars$vector_control) == 'setup'`
#' then dynamic vector control has been turned
#' on. The unmodified **`xds`** object is returned.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.setup = function(pars){
  return(pars)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(pars$vector_control) == 'dynamic'`
#' then dynamic vector control has been turned
#' on. The unmodified **`xds`** object is returned.
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_vector_control.dynamic = function(pars){
  return(pars)
}


