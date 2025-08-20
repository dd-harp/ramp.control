
#' @title Implement Vector Control
#' @description Implements various forms
#' of vector control. Each mode for vector
#' control is set up and configured separately.
#' @note This a junction to implement various modes of
#' vector control.
#' Vector control modules require
#' [**`ramp.control`**](https://github.com/dd-harp/ramp.control).
#' @inheritParams ramp.xds::VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, xds_obj) {
  xds_obj = BedNet(t, xds_obj)
  xds_obj = IRS(t, xds_obj)
  xds_obj = AreaSpray(t, xds_obj)
  xds_obj = SugarBaits(t, xds_obj)
  xds_obj = LSM(t, xds_obj)
  #  xds_obj = EM(t, xds_obj)
  #  xds_obj = Endectocide(t, xds_obj)
  #  xds_obj = ADLarvicide(t, xds_obj)
  return(xds_obj)
}

#' @title Vector Control for Static Vector Control
#' @description The `setup` case runs the `dynamic`
#' case once, to set the values of variables for
#' static models. It then reverts to `none` so that
#' those values are not changed again.
#' @inheritParams ramp.xds::VectorControl
#' @return a **`ramp.xds`** model object
#' @export
VectorControl.setup = function(t, y, xds_obj){
  class(xds_obj$vector_control) <- 'dynamic'
  xds_obj <- VectorControl(t, xds_obj)
  class(xds_obj$vector_control) <- 'none'
  return(xds_obj)
}


#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.dynamic <- function(t, y, xds_obj) {
  xds_obj <- BedNetCoverage(t, xds_obj)
  xds_obj <- IRSCoverage(t, xds_obj)
  xds_obj <- AreaSprayCoverage(t, xds_obj)
  xds_obj <- SugarBaitCoverage(t, xds_obj)
  xds_obj <- LSMCoverage(t, xds_obj)

  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- BedNetEffectSizes(t, xds_obj, s)
    xds_obj <- IRSEffectSizes(t, xds_obj, s)
    xds_obj <- AreaSprayEffectSizes(t, xds_obj, s)
    xds_obj <- SugarBaitEffectSizes(t, xds_obj, s)
    xds_obj <- LSMEffectSizes(t, xds_obj, s)
  }
  #  xds_obj = EM_EffectSizes(t, xds_obj)
  #  xds_obj = Endectocide_EffectSizes(t, xds_obj)
  #  xds_obj = ADLarvicide_EffectSizes(t, xds_obj)
  return(xds_obj)
}

#' @title Turn On Vector Control
#' @description Any function that sets up
#' non-trivial vector control must
#' calls this function.
#' If `class(xds_obj$vector_control) == 'none'`
#' then it is set to `dynamic` and the
#' trivial module for every mode of
#' vector control is set up. Otherwise,
#' nothing happens.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_vector_control = function(xds_obj){
  UseMethod("dynamic_vector_control", xds_obj$vector_control)
}

#' @title Turn On Vector Control
#' @description If
#' `class(xds_obj$vector_control) == 'none'`
#' then dynamic vector_control has not
#' been set up by any other function.
#' This sets `class(vector_control) <- 'dynamic'` and
#' then sets up a  trivial module for every mode of
#' vector control.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_vector_control.none = function(xds_obj){
  vector_control <- 'dynamic'
  class(vector_control) <- 'dynamic'
  xds_obj$vector_control <- vector_control
  xds_obj <- setup_no_bednets(xds_obj)
  xds_obj <- setup_no_irs(xds_obj)
  xds_obj <- setup_no_area_spray(xds_obj)
  xds_obj <- setup_no_lsm(xds_obj)
  xds_obj <- setup_no_sugar_baits(xds_obj)
  return(xds_obj)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(xds_obj$vector_control) == 'setup'`
#' then dynamic vector control has been turned
#' on. The unmodified **`ramp.xds`** model object is returned.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_vector_control.setup = function(xds_obj){
  return(xds_obj)
}

#' @title Vector Control is Turned On
#' @description If
#' `class(xds_obj$vector_control) == 'dynamic'`
#' then dynamic vector control has been turned
#' on. The unmodified **`ramp.xds`** model object is returned.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_vector_control.dynamic = function(xds_obj){
  return(xds_obj)
}


