
#' @title Setup Sugar Object
#' 
#' @description 
#' Setup an object to handle sugar availability 
#' and associated behaviors
#' 
#' @param xds_obj an **`xds`** model object 
#' 
#' @return an **`xds`** model object
#' 
#' @export
setup_sugar_object = function(xds_obj){
  sug <- list()
  class(sug) <- "static"
  xds_obj$variables$sugar_obj <- sug 
  
  xds_obj$variables$sugar = list() 
  xds_obj$variables$sugar[[1]] = rep(0, xds_obj$nPatches) 
  
  return(xds_obj)
}

#' @title Change Sugar Availability 
#' @description Change the value of sugar availability for *static* models 
#' @param sugar availability of other blood hosts 
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' @return an **`xds`** model object
#' @export
change_sugar = function(sugar, xds_obj,  s){
  stopifnot(length(sugar) == xds_obj$nPatches)
  xds_obj$variables$sugar[[s]] = sugar 
  return(xds_obj)
}

#' @title Sugar Dynamics 
#' 
#' @description This function sets the value of a term 
#' describing sugar availability 
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** object
#'
#' @return an **`xds`** object
#' @export
Sugar <- function(t, y, xds_obj) {
  UseMethod("Sugar", xds_obj$variables$sugar_obj)
}

#' @title Sugar Dynamics 
#' 
#' @description For static models, change nothing 
#' 
#' @inheritParams Sugar 
#' 
#' @return an **`xds`** object
#' @export
Sugar.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Sugar Availability 
#' 
#' @description Call `F_sugar` to set sugar availability dynamically
#'
#' @inheritParams Sugar 
#' 
#' @return an **`xds`** object
#' 
#' @export
Sugar.dynamic <- function(t, y, xds_obj) {
  xds_obj <- sugar_dynamics(t, y, xds_obj) 
  xds_obj <- sugartrap_dynamics(t, y, xds_obj)
  xds_obj <- sugartrap_availability(t, y, xds_obj) 
  return(xds_obj)
}

#' @title Sugar Dynamics 
#' 
#' @description This function computes 
#' availability of natural sugar sources dynamically
#' 
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** object
#'
#' @return an **`xds`** object
#' @export
sugar_dynamics <- function(t, y, xds_obj) {
  for(s in 1:xds_obj$nVectorSpecies){
    F_sug <- xds_obj$variables$sugar_obj[[s]]$F_sugar
    V <- get_variables(t, y, F_sug, xds_obj)
    xds_obj$variables$sugar[[s]] <- F_sug(t, V)  
  }
  return(xds_obj)
}

#' @title Set up no sugars
#' 
#' @description Setup a model for no time spent sugarsing 
#' and no exposure while sugarsing
#' 
#' @param mod_name the model name 
#' @param xds_obj an **`xds`** model object 
#' @param s the host species index
#' @param options  
#' 
#' @return an **`xds`** model object
#' @export
setup_F_sugar = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_sugar", mod_name) 
}

#' @title Set up no sugars
#' 
#' @description Setup a model for no time spent sugarsing 
#' and no exposure while sugarsing
#' 
#' @inheritParams setup_F_sugar 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_sugar.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$variables$sugar_obj) <- 'dynamic'
  FF <- make_ts_function(options) 
  FF -> xds_obj$variables$sugar_obj[[s]]$F_sugar  
  return(xds_obj) 
}

