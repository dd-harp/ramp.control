
#' @title The `setup` case for health variables
#' @description Call all the functions to set the
#' values of health variables and then revert
#' the `none` case
#' @inheritParams ramp.xds::Health
#' @return an **`xds`** object
Health.setup = function(t, y, pars){
  class(pars$health) <- 'dynamic'
  pars <- Health(t, y, pars)
  class(pars$health) <- 'none'
  return(pars)
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
#' @return an **`xds`** object
#' @seealso [dynamic_health]
Health.dynamic = function(t, y, pars){
  pars <- Clinic(t, y, pars)
  pars <- School(t, pars)
  pars <- MassHealth(t, pars)
  #  pars <- Behavior(t, pars)
  pars <- ActiveCaseDetection(t, y, pars)
  return(pars)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
dynamic_health = function(pars){
  UseMethod("dynamic_health", pars$health)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_health.none = function(pars){
  health <- 'dynamic'
  class(health) <- 'dynamic'
  pars$health <- health
  pars <- setup_no_clinic(pars)
  pars <- setup_no_school(pars)
  pars <- setup_no_mass_health(pars)
  #  pars <- setup_no_behavior(pars)
  pars <- setup_no_active_case_detection(pars)
  return(pars)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_health.setup = function(pars){
  return(pars)
}

#' @title Set up dynamic health
#' @description If dynamic health has not
#' already been set up, then turn on dynamic
#' health and set all the
#' @param pars an **`xds`** object
#' @return an **`xds`** object
#' @export
dynamic_health.dynamic = function(pars){
  return(pars)
}
