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
  N <- xds_obj$events_obj$bednet$N

  contact <-  rep(0, N)
  contact <-  with(options, contact)
  xds_obj$events_obj$bednet$contact = contact

  pw <-  rep(1, N)
  pw <-  with(options, pw)
  xds_obj$events_obj$bednet$pw = pw

  xds_obj$bednet_obj$contact_obj$F_contact = make_bednet_multiround(xds_obj, contact, pw)

  return(xds_obj)
}

#' @title Setup Muti-Round bednet Contact
#'
#' @description
#' With multi-round bednet contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param contact the new contact parameter
#'
#' @return a **`ramp.xds`**  model object
#'
#' @export
change_bednet_contact_multiround = function(xds_obj, contact){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))
  stopifnot(length(contact) == xds_obj$events_obj$bednet$N)
  contact -> xds_obj$events_obj$bednet$contact
  xds_obj$events_obj$bednet$pw -> pw
  xds_obj$bednet_obj$contact_obj$F_contact = make_bednet_multiround(xds_obj, contact, pw)
  return(xds_obj)
}

#' @title Setup Muti-Round bednet Contact
#'
#' @description
#' With multi-round bednet contact, there
#' is a different relationship between coverage
#' and contact in each round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param d_50 the new half saturation parameter
#'
#' @return a **`ramp.xds`**  model object
#'
#' @export
change_bednet_contact_d_50_multiround = function(xds_obj, d_50){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))
  stopifnot(length(d_50) == xds_obj$events_obj$bednet$N)
  d_50 -> xds_obj$events_obj$bednet$d_50
  xds_obj$events_obj$bednet$contact -> contact
  xds_obj$events_obj$bednet$pw -> pw
  xds_obj$bednet_obj$contact_obj$F_contact = make_bednet_multiround(xds_obj, contact, pw)
  return(xds_obj)
}

#' @title Setup Muti-Round bednet Shape
#'
#' @description
#' With multi-round bednet pw, there
#' is a different relationship between coverage
#' and pw in each round
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param pw the new shape parameter
#'
#' @return a **`ramp.xds`**  model object
#'
#' @export
change_bednet_pw_multiround = function(xds_obj, pw){
  stopifnot(with(xds_obj, exists("events_obj")))
  stopifnot(with(xds_obj$events_obj, exists("bednet")))
  stopifnot(length(pw) == xds_obj$events_obj$bednet$N)
  pw -> xds_obj$events_obj$bednet$pw
  xds_obj$events_obj$bednet$contact -> contact
  xds_obj$bednet_obj$conatct_obj$F_contact = make_bednet_multiround(xds_obj, contact, pw)
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
