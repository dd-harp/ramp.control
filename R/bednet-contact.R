
#' @title Set Up the Bed Net Contact Object
#'
#' @description Set up an object to model
#' bed net effective contact.
#'
#' @param name the model name
#' @param xds_obj a **`ramp.xds`**  model object
#' @param options a list of options to override defaults
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_bednet_contact = function(name, xds_obj, options=list()){
  class(name) <- name
  UseMethod("setup_bednet_contact", name)
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
setup_bednet_contact.none = function(name, xds_obj, options=list()){
  xds_obj$bednet_obj$contact_obj = make_none_object()
  return(xds_obj)
}


#' @title Get bednet coverage
#'
#' @description Get the values from a model
#' that set maximum bednet coverage
#'
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns a **`ramp.xds`** model object
#'
#' @export
get_bednet_contact = function(xds_obj){
  xds_obj$bednet_obj$contact
}

#' @title Get bednet coverage
#'
#' @description Get the values from a model
#' that set maximum bednet coverage
#'
#' @param coverage coverage parameters
#' @param xds_obj  a **`ramp.xds`** model object
#'
#' @returns maximum bednet coverage, as a vector
#'
#' @export
change_bednet_contact = function(coverage, xds_obj){
  UseMethod("change_bednet_contact", xds_obj$bednet_obj$contact_obj)
}

#' @title Set the cover_bednets
#'
#' @description Set the value of exogenous variables related to
#' cover_bednets
#'
#' @param t current simulation time
#' @param y state variables
#' @param xds_obj a **`ramp.xds`** model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Contact <- function(t, y, xds_obj) {
  UseMethod("Bed_Net_Contact", xds_obj$bednet_obj$contact_obj)
}

#' @title Set no cover_bednets
#'
#' @description The null model for cover_bednets
#'
#' @inheritParams Bed_Net_Contact
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_Contact.none <- function(t, y, xds_obj) {
  return(xds_obj)
}


#' Plot Bednet Contact
#'
#' @param tt a set of time points
#' @param xds_obj a **`ramp.xds`** model object
#' @param clr plotting color
#' @param add add to an existing plot
#'
#' @returns a **`ramp.xds`** xds_obj object
#' @export
show_bednet_contact = function(tt, xds_obj, clr="black", add=FALSE){
  y <- get_inits(xds_obj, flatten=TRUE)
  xds_obj <- Bed_Net_Coverage(tt, y, xds_obj)
  xds_obj <- Bed_Net_Contact(tt, y, xds_obj)
  if(add==FALSE)
    graphics::plot(tt, xds_obj$bednet_obj$contact, type = "n", xlab="Time (Days)", ylab = "Contact")
  graphics::lines(tt, xds_obj$bednet_obj$contact, col=clr)
}
