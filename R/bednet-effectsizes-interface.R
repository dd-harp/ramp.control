# see examples in betnet-effectsizes-func.R
# see examples in betnet-effectsizes-lemenach.R


#' @title Set up dynamic forcing
#' @description If dynamic forcing has not
#' already been set up, then turn on dynamic
#' forcing and set all the
#' @param name the name of a model to set up
#' @param pars an **`xds`** object
#' @param opts a list of options to override defaults
#' @return an **`xds`** object
#' @export
setup_bednet_effectsizes = function(name, pars, opts=list()){
  class(name) <- name
  UseMethod("setup_bednet_effectsizes", name)
}

