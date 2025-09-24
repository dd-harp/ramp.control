
#' @title Set Up the Null Bednet Model Object
#'
#' @description
#' Sets up the null bednet model object
#'
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
setup_bednet_object = function(xds_obj){
  xds_obj$bednet_obj = make_none_object()
  xds_obj <- setup_bednet_own("none", xds_obj, list())
  xds_obj <- setup_bednet_use("none", xds_obj, list())
  xds_obj <- setup_bednet_effects("none", xds_obj, list())
  xds_obj <- setup_bednet_coverage("none", xds_obj, list())
  xds_obj$bednet_obj$eff_sz_obj <- list()
  xds_obj <- setup_bednet_effect_sizes("none", xds_obj, 1, list())
  return(xds_obj)
}

#' @title Set Up a Bed Net Model
#'
#' @description This sets up a bed net
#' model
#'
#' @param xds_obj a **`ramp.xds`**  model object
#' @param own_name the name of a model for bed net ownership
#' @param own_opts options for the bed net ownership model
#' @param use_name the name of a model for bed net usage
#' @param use_opts options for the bed net usage model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param effect_sizes_name the name of a model for bed net effect sizes
#' @param effect_sizes_opts options for the bed net effect sizes model
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_bednets = function(xds_obj,
                        own_name = 'none', own_opts = list(),
                        use_name = 'none', use_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        effect_sizes_name = 'none', effect_sizes_opts = list()){

  xds_obj <- setup_vector_control(xds_obj)
  bednet <- list()
  class(bednet) <- 'static'
  bednet$coverage <- rep(0, xds_obj$nPatches)
  bednet$eff_sz_obj <- list()
  xds_obj$bednet_obj <- bednet
  xds_obj <- setup_bednet_own(own_name, xds_obj, own_opts)
  xds_obj <- setup_bednet_use(use_name, xds_obj, use_opts)
  xds_obj <- setup_bednet_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_bednet_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_bednet_effect_sizes(effect_sizes_name, xds_obj, 1, effect_sizes_opts)

  return(xds_obj)
}


#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net <- function(t, xds_obj){
  UseMethod("Bed_Net", xds_obj$bednet_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net.none <- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net.static<- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net.dynamic <- function(t, xds_obj){
  xds_obj <- Own_Bed_Net(t, xds_obj)
  xds_obj <- Use_Bed_Net(t, xds_obj)
  xds_obj <- Bed_Net_Effects(t, xds_obj)
  return(xds_obj)
}



