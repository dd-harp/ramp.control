#' @title Set up Vector Control
#'
#' @description Any function that sets up
#' vector control calls this function.
#'
#' The cases are:
#' + `none` there is an empty object
#' + `static` an object that needs to be initialized
#' + `setup` an object that must be initialized
#' + `dynamic` an object that must be evaluated every time
#'
#' If any function sets `vector_control_obj` to `dynamic` then
#' every other
#' trivial module for every mode of
#' vector control is set up. Otherwise,
#' nothing happens.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_vector_control = function(xds_obj){
  UseMethod("setup_vector_control", xds_obj$vector_control_obj)
}

#' @title Set Up Vector Control
#' @description If
#' `class(xds_obj$vector_control_obj) == 'none'`
#' this function sets up the
#' objects for all implemented modes of vector control.
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_vector_control.none = function(xds_obj){
  class(xds_obj$vector_control_obj) <- 'static'
  xds_obj <- setup_bednet_object(xds_obj)
  xds_obj <- setup_irs_object(xds_obj)
  xds_obj <- setup_area_spray_object(xds_obj)
  xds_obj <- setup_lsm_object(xds_obj)
  xds_obj <- setup_atsb_object(xds_obj)
  return(xds_obj)
}

#' @title Vector Control is Static
#'
#' @description
#'
#' If `class(xds_obj$vector_control) == 'static'`
#' then vector control has already been set up.
#'
#' This changes the class to to 'setup' to force
#' the model to run `dynamic_vector_control` once.
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_vector_control.static = function(xds_obj){
  return(xds_obj)
}

#' @title Vector Control is in Setup Mode
#'
#' @description
#' If `class(xds_obj$vector_control) == 'setup'`
#' then some other mode of vector control has already
#' changed the setup. Nothing needs to be changed.
#'  The unmodified **`ramp.xds`** model object is returned.
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_vector_control.setup = function(xds_obj){
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
setup_vector_control.dynamic = function(xds_obj){
  return(xds_obj)
}



#' @title Vector Control Dynamics
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
dynamic_vector_control = function(t, y, xds_obj){
  xds_obj = Bed_Net(t, xds_obj)
  xds_obj = IRS(t, xds_obj)
#  xds_obj = AreaSpray(t, xds_obj)
#  xds_obj = SugarBaits(t, xds_obj)
#  xds_obj = LSM(t, xds_obj)
}

#' @title Dynamic Vector Control
#' @description Calls `dynamic_vector_control`
#'
#' @note This the function that implements various modes of
#' vector control.
#'
#' @inheritParams ramp.xds::VectorControl
#' @return a named [list]
#' @export
VectorControl.dynamic <- function(t, y, xds_obj) {
  return(dynamic_vector_control(t, y, xds_obj))
  return(xds_obj)
}

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
VectorControl.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Vector Control for Static Vector Control
#'
#' @description
#' The `setup` case runs the `dynamic_vector_control`
#' and `dynamic_vc_effect_sizes` once, to ensure
#' that everything is configured
#' properly for static models. It then changes the
#' case back to `static`
#'
#' This calls both functions, so there
#' is no `setup` method for `VectorControlEffectSizes`
#' is needed
#'
#' @inheritParams ramp.xds::VectorControl
#' @return a **`ramp.xds`** model object
#' @export
VectorControl.setup = function(t, y, xds_obj){
  xds_obj <- dynamic_vector_control(t, y, xds_obj)
  xds_obj <- dynamic_vc_effect_sizes(t, y, xds_obj)
  class(xds_obj$vector_control_obj) <- 'static'
  return(xds_obj)
}

#' @title Vector Control Dynamics
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
dynamic_vc_effect_sizes = function(t, y, xds_obj){

  xds_obj <- Bed_Net_Coverage(t, y, xds_obj)
  xds_obj <- IRS_Coverage(t, y, xds_obj)
#  xds_obj <- AreaSprayCoverage(t, xds_obj)
#  xds_obj <- SugarBaitCoverage(t, xds_obj)
#  xds_obj <- LSMCoverage(t, xds_obj)

  for(s in 1:xds_obj$nVectorSpecies){
    xds_obj <- Bed_Net_Effect_Sizes(t, y, xds_obj, s)
    xds_obj <- IRS_Effect_Sizes(t, y, xds_obj, s)
#    xds_obj <- AreaSprayEffectSizes(t, xds_obj, s)
#    xds_obj <- SugarBaitEffectSizes(t, xds_obj, s)
#    xds_obj <- LSMEffectSizes(t, xds_obj, s)
  }
  return(xds_obj)
}

#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.dynamic <- function(t, y, xds_obj) {
  return(dynamic_vc_effect_sizes(t, y, xds_obj))
}

#' @title Distribute vector control, the null model
#' @description Implements [VectorControl] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControlEffectSizes
#' @return a named [list]
#' @export
VectorControlEffectSizes.static <- function(t, y, xds_obj) {
  return(xds_obj)
}
