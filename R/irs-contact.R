
#' @title Set Up the IRS Contact Object
#'
#' @description Set up an object to model
#' the effects of IRS, usually through the
#' computation of some intermediate variable
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_irs_contact = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_irs_contact", name)
}

#' @title Set Up the Null IRS Contact Object
#'
#' @description
#' Set up a model for IRS effects that does nothing.
#'
#' @inheritParams setup_irs_contact
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_irs_contact.none = function(name, xds_obj, options=list()){
  xds_obj$irs_obj$contact_obj = make_none_object()
  xds_obj$irs_obj$contact = list()
  return(xds_obj)
}

#' @title Get irs contact
#'
#' @description Get the values from a model
#' that set maximum irs contact
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
get_irs_contact = function(xds_obj){
  xds_obj$irs_obj$contact
}

#' @title Get irs contact
#'
#' @description Get the values from a model
#' that set maximum irs contact
#'
#' @param contact contact parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns maximum irs contact, as a vector
#'
#' @export
change_irs_contact = function(contact, xds_obj){
  UseMethod("get_irs_contact", xds_obj$irs_obj$contact_obj)
}


#' Plot IRS Contact
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_irs_contact = function(tt, xds_obj, clr="black", add=FALSE){
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- IRS_Contact(tt, y, xds_obj)
  if(add==FALSE)
    graphics::plot(tt, xds_obj$irs_obj$contact, type = "n", xlab="Time (Days)", ylab = "Contact")
  graphics::lines(tt, xds_obj$irs_obj$contact, col=clr)
}

#' @title IRS Contact
#'
#' @description Compute IRS contact
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
IRS_Contact <- function(t, y, xds_obj) {
  UseMethod("IRS_Contact", xds_obj$irs_obj$contact_obj)
}

#' @title Set no use_irss
#' @description The null model for use_irss
#' @inheritParams IRS_Contact
#' @return [list]
#' @export
IRS_Contact.none <- function(t, y, xds_obj) {
  return(xds_obj)
}
