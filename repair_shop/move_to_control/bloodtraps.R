
#' @title Setup the Blood Trap Object 
#' 
#' @description Setup the object to handle blood traps. 
#' 
#' A variable, `Btraps,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_bloodtrap`. 
#' 
#' The `trap_weights` are the search weights for each vector species
#' 
#' @param xds_obj an **`xds`** model object 
#' 
#' @return an **`xds`** model object
#' @export
setup_bloodtrap_object = function(xds_obj){
  
  ovi <- list()
  
  xds_obj$XY_interface$bloodtrap_obj <- ovi 
  xds_obj$XY_interface$Ntraps <- rep(0, xds_obj$nPatches)
  xds_obj$XY_interface$trap_weights <- list()
  xds_obj$XY_interface$trap_weights[[1]] = rep(1, xds_obj$nPatches)
  xds_obj$XY_interface$Btraps <- list()
  xds_obj$XY_interface$Btraps[[1]] =  rep(0, xds_obj$nPatches)
  
  xds_obj <- setup_F_bloodtrap("static", xds_obj, 1)
  
  return(xds_obj)
}

#' @title Setup the bloodtrap Object 
#' 
#' @description Setup the object to handle bloodtraps. 
#' 
#' A variable, `Btraps,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_bloodtrap`. 
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
get_N_bloodtraps = function(t, y, xds_obj, s){
  with(xds_obj$XY_interface,{
    vars = list(Ntraps <- Ntraps) 
    return(vars)
  })}

#' @title Change bloodtrap Availability
#' 
#' @description Set the availailbity of 
#' bloodtraps
#' 
#' @param Ntraps the number of bloodtraps  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_N_bloodtraps = function(Ntraps, xds_obj, s=1){
  stopifnot(length(Ntraps) == xds_obj$nPatches)
  xds_obj$XY_interface$trap_number <- Ntraps 
  class(xds_obj$XY_interface$bloodtrap_obj) <- 'setup'
  return(xds_obj)
}

#' @title Change bloodtrap Search Weights
#' @description Set the search weights, \eqn{\omega}, 
#' for a set of aquatic bloodtraps
#' 
#' @param wts bloodtrap search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_bloodtrap_weights = function(wts, xds_obj, s=1){
  stopifnot(length(wts) == xds_obj$nPatches)
  xds_obj$XY_interface$trap_weights[[s]] = wts 
  class(xds_obj$XY_interface$bloodtrap_obj) <- 'setup'
  return(xds_obj)
}

#' @title bloodtrap Availability 
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
Bloodtraps <- function(t, y, xds_obj){
  UseMethod("bloodtraps", xds_obj$XY_interface$bloodtrap_obj)
}

#' @title bloodtrap Availability 
#' 
#' @description For a static model for time spent F_bloodtrap, the function
#' does not update anything.
#' 
#' @inheritParams bloodtraps
#' @return an **`xds`** object
#' @export
Bloodtraps.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title bloodtrap Availability 
#' @description This function sets a static value for the parameter
#' describing time spent F_bloodtrap, resets the class to `static` and
#' then triggers an update to the blood feeding model.
#' It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' @inheritParams bloodtraps
#' @return an **`xds`** object
#' @export
Bloodtraps.setup <- function(t, y, xds_obj) {
  class(xds_obj$XY_interface$bloodtrap_obj) <- "static"
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  xds_obj <- bloodtrap_dynamics(t, y, xds_obj)
  return(xds_obj)
}

#' @title bloodtrap Availability 
#' 
#' @description This function sets the value of a parameter
#' describing time spent F_bloodtrap. It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' 
#' @inheritParams bloodtraps
#' @return an **`xds`** object
#' @export
Bloodtraps.dynamic <- function(t, y, xds_obj) {
  xds_obj <- bloodtrap_dynamics(t, y, xds_obj)
  xds_obj <- bloodtrap_availability(t, y, xds_obj)
  return(xds_obj) 
}

#' @title bloodtrap Dynamics 
#' 
#' @description bloodtrap dynamics
#' sets a variable describing the number of 
#' bloodtrap per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
bloodtrap_dynamics <- function(t, y, xds_obj){
  with(xds_obj$XY_interface,{
    F_trap <- bloodtrap_obj$F_bloodtrap
    V <- bloodtrap_obj$get_variables(t, y, xds_obj)
    Ntraps <- F_trap(t, V) 
  }) 
  return(xds_obj)    
}

#' @title bloodtrap Availability 
#' 
#' @description bloodtrap dynamics
#' sets a variable describing the number of 
#' bloodtrap per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
bloodtrap_availability <- function(t, y, xds_obj){
  Ntraps<- xds_obj$XY_interface$Ntraps 
  for(s in 1:xds_obj$nVectorSpecies){
    wts <- xds_obj$XY_interface$trap_weights[[s]]
    wts*Ntraps -> xds_obj$XY_interface$Btraps[[s]] 
  }
  return(xds_obj)    
}

#' @title Set up no bloodtrap
#' 
#' @description Setup a model for no time spent bloodtraping 
#' and no exposure while bloodtraping
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index
#' @param options setup options for F_bloodtrap 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_bloodtrap = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_bloodtrap", mod_name) 
}

#' @title Setup F_bloodtrap 
#' 
#' @description Setup a static model for  
#' availability of bloodtraps 
#' 
#' @inheritParams setup_F_bloodtrap
#' 
#' @return an **`xds`** model object
#' @export
setup_F_bloodtrap.static = function(mod_name, xds_obj, s, options){
  F_bloodtrap <- Zero_tV 
  F_bloodtrap$get_variables <- get_N_bloodtraps 
  xds_obj$XY_interface$bloodtrap_obj$F_bloodtrap = F_bloodtrap 
  return(xds_obj) 
}

#' @title Setup bloodtrap Availability Function 
#' 
#' @description Setup a function to model 
#' dynamic availability of bloodtrap 
#' 
#' @inheritParams setup_F_bloodtrap
#' 
#' @return an **`xds`** model object
#' @export
setup_F_bloodtrap.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$XY_interface$bloodtrap_obj) = 'dynamic'
  class(xds_obj$XY_interface) = 'dynamic'
  class(xds_obj$beta) = 'dynamic'
  xds_obj$XY_interface$bloodtrap_obj[[s]]$F_bloodtrap= make_ts_function(options) 
  return(xds_obj) 
}

