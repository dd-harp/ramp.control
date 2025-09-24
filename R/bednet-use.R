
#' @title Set Up the Bed Net Use Object
#'
#' @description Set up an object to model
#' bed net use
#'
#' @param name the model name
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to override defaults
#'
#' @return the **`xds`** model object
#' @export
setup_bednet_use = function(name, xds_obj, options=list()){
 class(name) <- name
 UseMethod("setup_bednet_use", name)
}

#' @title Set Up the Null Bed Net Use Object
#'
#' @description
#' Set up the null bed net use object
#'
#' @param name the name of a model to set up
#' @param xds_obj an **`xds`** model object
#' @param options a list of options to override defaults
#'
#' @return the **`xds`** model object
#'
#' @export
setup_bednet_use.none = function(name, xds_obj, options=list()){
 xds_obj$bednet_obj$use_obj = make_none_object()
 return(xds_obj)
}


#' @title Use Bed Nets
#'
#' @description Set the value of variables related to
#' bed net use
#'
#' @param t current simulation time
#' @param xds_obj an **`xds`** model object
#'
#' @return the **`xds`** model object
#' @export
Use_Bed_Net <- function(t, xds_obj) {
 UseMethod("Use_Bed_Net", xds_obj$bednet_obj$use_obj)
}

#' @title Null Bed Net Use Model
#' @description Do nothing
#' @inheritParams Use_Bed_Net
#' @return the **`xds`** model object
#' @export
Use_Bed_Net.none <- function(t, xds_obj) {
 return(xds_obj)
}
