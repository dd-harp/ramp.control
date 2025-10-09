
#' @title IRS Contact
#'
#' @description Set up a linear model
#' for contact relative to coverage
#'
#' @inheritParams IRS_Contact
#'
#' @return a **`ramp.xds`** model object
#' @export
IRS_Contact.linear <- function(t, y, xds_obj) {
  cp = xds_obj$irs_obj$contact_obj$contact_parameter
  xds_obj$irs_obj$contact = xds_obj$irs_obj$coverage*cp
  return(xds_obj)
}

#' @title Set Up the Null IRS Contact Object
#'
#' @description
#' Set up the null irs coverage object
#'
#' @param name the name of a model to set up
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_contact.linear= function(name, xds_obj, options=list()){
  contact_obj <- list()
  class(contact_obj) <- "linear"
  cp <- min(max(cp,0),1)
  contact_obj$contact_parameter <- with(options, cp)
  xds_obj$irs_obj$contact_obj = contact_obj
  xds_obj$irs_obj$contact = xds_obj$irs_obj$coverage*cp
  return(xds_obj)
}
