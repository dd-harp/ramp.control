
#' @title The `setup` case for health variables
#' @description Call all the functions to set the
#' values of health variables and then revert
#' the `none` case
#' @inheritParams ramp.xds::Health
#' @return a **`ramp.xds`** model object
Health.setup = function(t, y, xds_obj){
  class(xds_obj$health) <- 'dynamic'
  xds_obj <- Health(t, y, xds_obj)
  class(xds_obj$health) <- 'none'
  return(xds_obj)
}

#' @title Set the values of exogenous variables
#' @description With dynamic health, exogenous variables
#' can be set in one of four function calls:
#' - Clinic
#' - School
#' - MassHealth
#' - Behavior
#' - ActiveCaseDetection
#' @inheritParams ramp.xds::Health
#' @return a **`ramp.xds`** model object
#' @seealso [dynamic_health]
Health.dynamic = function(t, y, xds_obj){
  xds_obj <- Clinic(t, y, xds_obj)
  xds_obj <- School(t, xds_obj)
  xds_obj <- MassHealth(t, xds_obj)
  #  xds_obj <- Behavior(t, xds_obj)
  xds_obj <- ActiveCaseDetection(t, y, xds_obj)
  return(xds_obj)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
dynamic_health = function(xds_obj){
  UseMethod("dynamic_health", xds_obj$health)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_health.none = function(xds_obj){
  health <- 'dynamic'
  class(health) <- 'dynamic'
  xds_obj$health <- health
  xds_obj <- setup_no_clinic(xds_obj)
  xds_obj <- setup_no_school(xds_obj)
  xds_obj <- setup_no_mass_health(xds_obj)
  #  xds_obj <- setup_no_behavior(xds_obj)
  xds_obj <- setup_no_active_case_detection(xds_obj)
  return(xds_obj)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_health.setup = function(xds_obj){
  return(xds_obj)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param xds_obj a **`ramp.xds`** model object
#' @return a **`ramp.xds`** model object
#' @export
dynamic_health.dynamic = function(xds_obj){
  return(xds_obj)
}
