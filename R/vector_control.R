#' @title Set up Vector Control
#'
#' @description Any function that sets up
#' vector control calls this function.
#'
#' The cases are:
#' + `none` the object needs to be initialized
#' + `done` the object has been initialized
#'
#' If any function sets `vector_control_obj` to `dynamic` then
#' every other
#' trivial module for every mode of
#' vector control is set up. Otherwise,
#' nothing happens.
#'
#' @param xds_obj a **`ramp.xds`** model object
#'
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

#' @title Set Up Vector Control
#' @description If
#' `class(xds_obj$vector_control_obj) == 'none'`
#' this function sets up the
#' objects for all implemented modes of vector control.
#'
#' @keywords internal
#'
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
setup_vector_control.static = function(xds_obj){
  return(xds_obj)
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
setup_vector_control.dynamic = function(xds_obj){
  return(xds_obj)
}


#' @title Dynamic Vector Control
#' @description Calls `dynamic_vector_control`
#'
#' @note This the function that implements various modes of
#' vector control.
#'
#' @keywords internal
#'
#' @inheritParams ramp.xds::VectorControl1
#' @return a named [list]
#' @export
VectorControl1.dynamic <- function(t, y, xds_obj) {
  xds_obj = Bed_Net_1(t, y, xds_obj)
  xds_obj = IRS_1(t, y, xds_obj)
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
#' @inheritParams ramp.xds::VectorControl1
#' @return a named [list]
#' @export
VectorControl1.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Distribute vector control, the null model
#' @description Implements [VectorControl1] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControl2
#' @return a named [list]
#' @export
VectorControl2.dynamic <- function(t, y, xds_obj) {
  xds_obj = Bed_Net_2(t, y, xds_obj)
  xds_obj = IRS_2(t, y, xds_obj)
  return(xds_obj)
}

#' @title Distribute vector control, the null model
#' @description Implements [VectorControl1] for the control model of vector control (do nothing)
#' @inheritParams ramp.xds::VectorControl2
#' @return a named [list]
#' @export
VectorControl2.static <- function(t, y, xds_obj) {
  return(xds_obj)
}
