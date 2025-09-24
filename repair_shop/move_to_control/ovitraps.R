
#' @title Setup the Ovitrap Object 
#' 
#' @description Setup the object to handle ovitraps. 
#' 
#' A variable, `Qtraps,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_ovitrap`. 
#' 
#' The `trap_weights` are the search weights for each vector species
#' 
#' @param xds_obj an **`xds`** model object 
#' 
#' @return an **`xds`** model object
#' @export
setup_ovitrap_object = function(xds_obj){
  
  ovi <- list()
  
  xds_obj$ML_interface$ovitrap_obj <- ovi 
  xds_obj$ML_interface$Ntraps <- rep(0, xds_obj$nPatches)
  xds_obj$ML_interface$trap_weights <- list()
  xds_obj$ML_interface$trap_weights[[1]] = rep(1, xds_obj$nPatches)
  xds_obj$ML_interface$Qtraps <- list()
  xds_obj$ML_interface$Qtraps[[1]] =  rep(0, xds_obj$nPatches)
  
  xds_obj <- setup_F_ovitrap("static", xds_obj, 1)
  
  return(xds_obj)
}

#' @title Setup the Ovitrap Object 
#' 
#' @description Setup the object to handle ovitraps. 
#' 
#' A variable, `Qtraps,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_ovitrap`. 
#' 
#' The `trap_weights` are the search weights for each vector species
#' 
#' @param t current simulation time
#' @param y state vector
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index 
#' 
#' @return an **`xds`** model object
#' @export
get_N_ovitraps = function(t, y, xds_obj, s){
  with(xds_obj$ML_interface,{
    vars = list(Ntraps <- Ntraps) 
    return(vars)
})}

#' @title Change Ovitrap Availability
#' 
#' @description Set the availailbity of 
#' ovitraps
#' 
#' @param Ntraps the number of ovitraps  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_N_ovitraps = function(Ntraps, xds_obj, s=1){
  stopifnot(length(Ntraps) == xds_obj$nPatches)
  xds_obj$ML_interface$trap_number <- Ntraps 
  class(xds_obj$ML_interface$ovitrap_obj) <- 'setup'
  return(xds_obj)
}

#' @title Change Ovitrap Search Weights
#' @description Set the search weights, \eqn{\omega}, 
#' for a set of aquatic ovitraps
#' 
#' @param wts ovitrap search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_ovitrap_weights = function(wts, xds_obj, s=1){
  stopifnot(length(wts) == xds_obj$nPatches)
  xds_obj$ML_interface$trap_weights[[s]] = wts 
  class(xds_obj$ML_interface$ovitrap_obj) <- 'setup'
  return(xds_obj)
}

#' @title Ovitrap Availability 
#' 
#' @description This function sets the value of a term 
#' describing the availability of ovi traps
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#'
#' @return an **`xds`** object
#' @export
Ovitraps <- function(t, y, xds_obj){
  UseMethod("Ovitraps", xds_obj$ML_interface$ovitrap_obj)
}

#' @title Ovitrap Availability 
#' 
#' @description For a static model for time spent F_ovitrap, the function
#' does not update anything.
#' 
#' @inheritParams Ovitraps
#' @return an **`xds`** object
#' @export
Ovitraps.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Ovitrap Availability 
#' @description This function sets a static value for the parameter
#' describing time spent F_Ovitrap, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams Ovitraps
#' @return an **`xds`** object
#' @export
Ovitraps.setup <- function(t, y, xds_obj) {
  class(xds_obj$ML_interface$ovitrap_obj) <- "static"
  xds_obj$ML_interface <- trigger_setup(xds_obj$ML_interface)
  xds_obj <- ovitrap_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title Ovitrap Availability 
#' 
#' @description This function sets the value of a parameter
#' describing time spent F_Ovitrap. It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' 
#' @inheritParams Ovitraps
#' @return an **`xds`** object
#' @export
Ovitraps.dynamic <- function(t, y, xds_obj) {
  xds_obj <- ovitrap_dynamics(t, y, xds_obj)
  xds_obj <- ovitrap_availability(t, y, xds_obj)
  return(xds_obj) 
}

#' @title Ovitrap Dynamics 
#' 
#' @description Ovitrap dynamics
#' sets a variable describing the number of 
#' ovitrap per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
ovitrap_dynamics <- function(t, y, xds_obj){
  with(xds_obj$ML_interface,{
    F_trap <- ovitrap_obj$F_ovitrap
    V <- ovitrap_obj$get_variables(t, y, xds_obj)
    Ntraps <- F_trap(t, V) 
  }) 
  return(xds_obj)    
}

#' @title Ovitrap Availability 
#' 
#' @description Ovitrap dynamics
#' sets a variable describing the number of 
#' ovitrap per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
ovitrap_availability <- function(t, y, xds_obj){
  Ntraps<- xds_obj$ML_interface$Ntraps 
  for(s in 1:xds_obj$nVectorSpecies){
    wts <- xds_obj$ML_interface$trap_weights[[s]]
    wts*Ntraps -> xds_obj$ML_interface$Qtraps[[s]] 
  }
  return(xds_obj)    
}

#' @title Set up no ovitrap
#' 
#' @description Setup a model for no time spent ovitraping 
#' and no exposure while ovitraping
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index
#' @param options setup options for F_ovitrap 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_ovitrap = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_ovitrap", mod_name) 
}

#' @title Setup F_ovitrap 
#' 
#' @description Setup a static model for  
#' availability of ovitraps 
#' 
#' @inheritParams setup_F_ovitrap
#' 
#' @return an **`xds`** model object
#' @export
setup_F_ovitrap.static = function(mod_name, xds_obj, s, options){
  F_ovitrap <- Zero_tV 
  F_ovitrap$get_variables <- get_N_ovitraps 
  xds_obj$ML_interface$ovitrap_obj$F_ovitrap = F_ovitrap 
  return(xds_obj) 
}

#' @title Setup Ovitrap Availability Function 
#' 
#' @description Setup a function to model 
#' dynamic availability of ovitrap 
#' 
#' @inheritParams setup_F_ovitrap
#' 
#' @return an **`xds`** model object
#' @export
setup_F_ovitrap.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$ML_interface$ovitrap_obj) = 'dynamic'
  class(xds_obj$ML_interface) = 'dynamic'
  class(xds_obj$beta) = 'dynamic'
  xds_obj$ML_interface$ovitrap_obj[[s]]$F_ovitrap= make_ts_function(options) 
  return(xds_obj) 
}

