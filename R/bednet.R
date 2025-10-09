
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
  xds_obj <- setup_bednet_access("none", xds_obj, list())
  xds_obj <- setup_bednet_use("none", xds_obj, list())
  xds_obj <- setup_bednet_effects("none", xds_obj, list())
  xds_obj <- setup_bednet_coverage("none", xds_obj, list())
  xds_obj <- setup_bednet_contact("none", xds_obj, list())
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
#' @param access_name the name of a model for bed net access
#' @param access_opts options for the bed net access model
#' @param use_name the name of a model for bed net usage
#' @param use_opts options for the bed net usage model
#' @param effects_name the name of a model for bed net effects
#' @param effects_opts options for the bed net effects model
#' @param coverage_name the name of a model for bed net coverage
#' @param coverage_opts options for the bed net coverage model
#' @param contact_name the name of a model for bed net contact
#' @param contact_opts options for the bed net contact model
#' @param effect_sizes_name the name of a model for bed net effect sizes
#' @param effect_sizes_opts options for the bed net effect sizes model
#'
#' @return a **`ramp.xds`** model object
#' @export
setup_bednets = function(xds_obj,
                        access_name = 'none', access_opts = list(),
                        use_name = 'none', use_opts = list(),
                        effects_name = 'none', effects_opts = list(),
                        coverage_name = 'none', coverage_opts = list(),
                        contact_name = 'none', contact_opts = list(),
                        effect_sizes_name = 'none', effect_sizes_opts = list()){

  xds_obj <- setup_vector_control(xds_obj)
  bednet <- list()
  class(bednet) <- 'static'
  bednet$coverage <- rep(0, xds_obj$nPatches)
  bednet$eff_sz_obj <- list()
  xds_obj$bednet_obj <- bednet
  xds_obj <- setup_bednet_access(access_name, xds_obj, access_opts)
  xds_obj <- setup_bednet_use(use_name, xds_obj, use_opts)
  xds_obj <- setup_bednet_effects(effects_name, xds_obj, effects_opts)
  xds_obj <- setup_bednet_coverage(coverage_name, xds_obj, coverage_opts)
  xds_obj <- setup_bednet_contact(contact_name, xds_obj, contact_opts)
  xds_obj <- setup_bednet_effect_sizes(effect_sizes_name, xds_obj, 1, effect_sizes_opts)

  return(xds_obj)
}


#' @title Bednet, Stage 1
#' @description Set the value of exogenous variables related to
#' bed nets
#'
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_1 <- function(t, xds_obj){
  UseMethod("Bed_Net_1", xds_obj$bednet_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_1.none <- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_1.static<- function(t, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#'
#' @param t current simulation time
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_1.dynamic <- function(t, xds_obj){
  xds_obj <- Bed_Net_Access(t, xds_obj)
  xds_obj <- Use_Bed_Net(t, xds_obj)
  xds_obj <- Bed_Net_Effects(t, xds_obj)
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#'
#' @param t current simulation time
#' @param y state variables vector
#' @param xds_obj a **`ramp.xds`**  model object
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_2 <- function(t, y, xds_obj){
  UseMethod("Bed_Net_2", xds_obj$bednet_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @inheritParams Bed_Net_2
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_2.none <- function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#' @inheritParams Bed_Net_2
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_2.static<- function(t, y, xds_obj){
  return(xds_obj)
}

#' @title Set the bednet
#' @description Set the value of exogenous variables related to
#' bednet
#'
#' @inheritParams Bed_Net_2
#'
#' @return a **`ramp.xds`** model object
#' @export
Bed_Net_2.dynamic <- function(t, y, xds_obj){
  xds_obj <- Bed_Net_Coverage(t, y, xds_obj)
  xds_obj <- Bed_Net_Contact(t, y, xds_obj)
  for(s in 1:xds_obj$nVectorSpecies)
    xds_obj <- Bed_Net_Effect_Sizes(t, y, xds_obj, s)
  return(xds_obj)
}






