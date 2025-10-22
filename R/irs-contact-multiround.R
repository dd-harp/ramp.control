# Irs Multi-Round configures

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
  contact_obj <- list()
  contact_obj$class = "multiround"
  class(contact_obj) = "multiround"
  xds_obj$irs_obj$contact_obj = contact_obj
  N <- xds_obj$events_obj$irs$N

  contact <-  rep(0, N)
  contact <-  with(options, contact)
  xds_obj$events_obj$irs$contact = contact

  pw <-  rep(1, N)
  pw <-  with(options, pw)
  xds_obj$events_obj$irs$pw = pw

  xds_obj$irs_obj$contact_obj$F_contact = make_irs_multiround(xds_obj, contact, pw)
  return(xds_obj)
}

#' @title Setup Muti-Round IRS Contact
#'
#' @description
#' With multi-round irs contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param contact the new contact parameter
#'
#' @return a **`ramp.xds`**  model object
#'
#' @export
change_irs_contact_multiround = function(xds_obj, contact){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))
  stopifnot(length(contact) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$contact = contact
  pw <- xds_obj$events_obj$irs$pw
  xds_obj$irs_obj$contact_obj$F_contact = make_irs_multiround(xds_obj, contact, pw)
  return(xds_obj)
}

#' @title Setup Muti-Round IRS Shape
#'
#' @description
#' With multi-round irs pw, there
#' is a different relationship between coverage
#' and pw in each round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param pw the new shape parameter
#'
#' @return a **`ramp.xds`**  model object
#'
#' @export
change_irs_pw_multiround = function(xds_obj, pw){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("irs")))
  stopifnot(length(pw) == xds_obj$events_obj$irs$N)
  xds_obj$events_obj$irs$pw = pw
  contact <- xds_obj$events_obj$irs$contact
  xds_obj$irs_obj$conatct_obj$F_contact = make_irs_multiround(xds_obj, contact, pw)
  return(xds_obj)
}



#' @title IRS Contact for Multiround Models
#'
#' @description A model for IRS contact over time
#' when there have been several rounds of IRS
#'
#' @inheritParams IRS_Contact
#' @return a **`ramp.xds`** model object
#' @export
IRS_Contact.multiround <- function(t, y, xds_obj) {
  with(xds_obj$irs_obj$contact_obj,{
    xds_obj$irs_obj$contact = F_contact(t)
    return(xds_obj)
  })}

