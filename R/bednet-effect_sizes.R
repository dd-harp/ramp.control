
#' @title Set Up the Bed Net Effect Sizes Object
#'
#' @description
#' Set up a model object to compute
#' an effect sizes of bed nets
#'
#' @param name the bed net effect sizes model name
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @param options a list of options to override defaults
#'
#' @return the **`xds`** model object
#' @export
setup_bednet_effect_sizes = function(name, xds_obj, s=1, options=list()){
 class(name) <- name
 UseMethod("setup_bednet_effect_sizes", name)
}

#' @title Set Up the Null Model for Bed Net Effect Sizes
#'
#' @description
#' Set up the null bed net effect sizes model object
#'
#' @inheritParams setup_bednet_effect_sizes
#'
#' @return the **`xds`** model object
#'
#' @export
setup_bednet_effect_sizes.none = function(name, xds_obj, s=1, options=list()){
 xds_obj$bednet_obj$eff_sz_obj[[s]] = make_none_object()
 return(xds_obj)
}

#' @title Bed Net Effect Sizes
#'
#' @description
#' Compute bed net effect sizes
#'
#' @param t current simulation time
#' @param y state variables vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return the **`xds`** model object
#' @export
Bed_Net_Effect_Sizes <- function(t, y, xds_obj, s=1){
 UseMethod("Bed_Net_Effect_Sizes", xds_obj$bednet_obj$eff_sz_obj[[s]])
}

#' @title The null Model for Bed Net Effect Sizes
#'
#' @description Do nothing
#'
#' @param t current simulation time
#' @param y state variables vector
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index

#' @return the **`xds`** model object
#' @export
Bed_Net_Effect_Sizes.none <- function(t, y, xds_obj, s=1){
 return(xds_obj)
}



