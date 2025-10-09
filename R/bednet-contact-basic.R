
#' @title Bed Net Contact
#'
#' @description Set up a linear model
#' for contact relative to coverage
#'
#' @inheritParams Bed_Net_Contact
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Contact.linear <- function(t, y, xds_obj) {
  cp = xds_obj$bednet_obj$contact_obj$contact_parameter
  xds_obj$bednet_obj$contact = xds_obj$bednet_obj$coverage*cp
  return(xds_obj)
}

#' @title Set Up the Null Bed Net Contact Object
#'
#' @description
#' Set up the null bed net coverage object
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_bednet_contact.linear= function(name, xds_obj, options=list()){
  contact_obj <- list()
  class(contact_obj) <- "linear"
  contact_obj$contact_parameter <- with(options, min(max(cp,0),1))
  xds_obj$bednet_obj$contact_obj = contact_obj
  xds_obj$bednet_obj$contact = xds_obj$bednet_obj$coverage
  return(xds_obj)
}
