# Irs Multi-Round configures F_cover

#' @title Set up dynamic forcing
#'
#' @description A set up utility function for
#' [IRS_Contact].
#'
#' @inheritParams setup_irs_contact
#'
#' @return a IRS contact model object
#' @export
setup_irs_contact.multiround = function(name, xds_obj, options=list()){
  class(xds_obj$vector_control_obj) = "dynamic"
  class(xds_obj$irs_obj) = "dynamic"
  xds_obj$irs_obj$contact_obj <- make_irs_multiround(xds_obj, use_contact=TRUE)
  return(xds_obj)
}

#' @title Setup Muti-Round IRS Contact
#'
#' @description
#' With multi-round irs contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#' @inheritParams change_irs_contact
#'
#' @export
change_irs_contact_multiround = function(contact, xds_obj){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))
  stopifnot(length(contact) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$contact = contact
  xds_obj$irs_obj$contact_obj <- make_irs_multiround(xds_obj, use_contact=TRUE)
  return(xds_obj)
}


#' @title IRS contact with Multiple Rounds
#' @description A model for IRS contact over time
#' when there have been several, different IRS models
#' @inheritParams IRS_Contact
#' @return a **`ramp.xds`** model object
#' @export
IRS_Contact.multiround <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$contact_obj,{
    xds_obj$irs_obj$contact = F_cover(t)
    return(xds_obj)
  })}

