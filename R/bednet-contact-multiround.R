# Bed Net Multi-Round Contact

#' @title Setup Muti-Round Bed Net Contact
#'
#' @description
#' With multi-round bed net contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#'
#' @inheritParams setup_bednet_contact
#' @export
setup_bednet_contact.multiround = function(name="multiround", xds_obj, options=list()){
  class(xds_obj$vector_control_obj) <- "dynamic"
  class(xds_obj$bednet_obj) <- "dynamic"
  xds_obj$bednet_obj$contact_obj = list()
  class(xds_obj$bednet_obj$contact_obj) = "multiround"
  contact <- with(xds_obj$events_obj$bednet, with(options, contact))
  xds_obj <- change_bednet_contact_multiround(contact, xds_obj)
  return(xds_obj)
}

#' @title Setup Muti-Round Bed Net Contact
#'
#' @description
#' With multi-round bed net contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#' @param contact the contact parametter
#' @param xds_obj an **`xds`** model object
#'
#' @export
change_bednet_contact_multiround = function(contact, xds_obj){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))
  stopifnot(length(contact) == xds_obj$events_obj$bednet$N)
  xds_obj$events_obj$bednet$contact = contact
  xds_obj$bednet_obj$contact_obj$F_contact <- make_bednet_multiround(contact, xds_obj)
  return(xds_obj)
}

#' @title Set no bednet_coverage
#' @description The null model for bednet_coverage
#' @inheritParams Bed_Net_Contact
#' @return a **`xds`** object
#' @export
Bed_Net_Contact.multiround <- function(t, y, xds_obj) {
  with(xds_obj$bednet_obj$contact_obj,{
    xds_obj$bednet_obj$contact = F_contact(t)
    return(xds_obj)
  })}
