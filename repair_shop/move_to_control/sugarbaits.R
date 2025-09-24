
#' @title Setup the Sugar Bait Object 
#' 
#' @description Setup the object to handle sugar baits 
#' 
#' A variable, `Sbaits,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_sugarbait`. 
#' 
#' The `trap_weights` are the search weights for each vector species
#' 
#' @param xds_obj an **`xds`** model object 
#' 
#' @return an **`xds`** model object
#' @export
setup_sugarbait_object = function(xds_obj){
  
  sbaits <- list()
  
  sbaits$Ntraps <- rep(0, xds_obj$nPatches)
  sbaits$trap_weights <- list()
  sbaits$weights[[1]] = rep(1, xds_obj$nPatches)
  xds_obj$variables$sugarbait_obj <- sbaits 
  
  xds_obj$variables$Sbaits <- list()
  xds_obj$variables$Sbaits[[1]] =  rep(0, xds_obj$nPatches)
  
  xds_obj <- setup_F_sugarbait("static", xds_obj, 1)
  
  return(xds_obj)
}

#' @title Setup the sugarbait Object 
#' 
#' @description Setup the object to handle sugarbaits. 
#' 
#' A variable, `Sbaits,` describes trap availability, 
#' essentially the number of traps, per patch, set by `F_sugarbait`. 
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
get_N_sugarbaits = function(t, y, xds_obj, s){
  with(xds_obj$variables$sugarbait_obj,{
    vars = list(Ntraps <- Ntraps) 
    return(vars)
  })}

#' @title Change sugarbait Availability
#' 
#' @description Set the availailbity of 
#' sugarbaits
#' 
#' @param Ntraps the number of sugarbaits  
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_N_sugarbaits = function(Ntraps, xds_obj, s=1){
  stopifnot(length(Ntraps) == xds_obj$nPatches)
  xds_obj$variables$sugarbait_obj$trap_number <- Ntraps 
  class(xds_obj$variables$sugarbait_obj$sugarbait_obj) <- 'setup'
  return(xds_obj)
}

#' @title Change sugarbait Search Weights
#' @description Set the search weights, \eqn{\omega}, 
#' for a set of aquatic sugarbaits
#' 
#' @param wts sugarbait search weights
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#' 
#' @return an **`xds`** model object
#' @export
change_sugarbait_weights = function(wts, xds_obj, s=1){
  stopifnot(length(wts) == xds_obj$nPatches)
  xds_obj$variables$sugarbait_obj$trap_weights[[s]] = wts 
  class(xds_obj$variables$sugarbait_obj$sugarbait_obj) <- 'setup'
  return(xds_obj)
}



#' @title sugarbait Availability 
#' 
#' @description This function sets the value of a parameter
#' describing time spent F_sugarbait. It is computed in [make_TaR]
#' and used to compute availability. It is also used in [Exposure]
#' 
#' @inheritParams sugarbaits
#' @return an **`xds`** object
#' @export
sugarbaits.dynamic <- function(t, y, xds_obj) {
  xds_obj <- sugarbait_dynamics(t, y, xds_obj)
  xds_obj <- sugarbait_availability(t, y, xds_obj)
  return(xds_obj) 
}

#' @title sugarbait Dynamics 
#' 
#' @description sugarbait dynamics
#' sets a variable describing the number of 
#' sugarbait per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
sugarbait_dynamics <- function(t, y, xds_obj){
  with(xds_obj$variables$sugarbait_obj,{
    F_trap <- sugarbait_obj$F_sugarbait
    V <- sugarbait_obj$get_variables(t, y, xds_obj)
    Ntraps <- F_trap(t, V) 
  }) 
  return(xds_obj)    
}

#' @title sugarbait Availability 
#' 
#' @description sugarbait dynamics
#' sets a variable describing the number of 
#' sugarbait per patch. Each species is
#' assigned a set of search weights. 
#' 
#' 
#' @param t current time
#' @param y state variables 
#' @param xds_obj an **`xds`** object
#' 
#' @return an **`xds`** object
#' @export
sugarbait_availability <- function(t, y, xds_obj){
  Ntraps<- xds_obj$variables$sugarbait_obj$Ntraps 
  for(s in 1:xds_obj$nVectorSpecies){
    wts <- xds_obj$variables$sugarbait_obj$trap_weights[[s]]
    wts*Ntraps -> xds_obj$variables$sugarbait_obj$Sbaits[[s]] 
  }
  return(xds_obj)    
}

#' @title Set up no sugarbait
#' 
#' @description Setup a model for no time spent sugarbaiting 
#' and no exposure while sugarbaiting
#' 
#' @param mod_name the model name
#' @param xds_obj an **`xds`** model object 
#' @param s the vector species index
#' @param options setup options for F_sugarbait 
#' 
#' @return an **`xds`** model object
#' @export
setup_F_sugarbait = function(mod_name, xds_obj, s, options){
  class(mod_name) = mod_name
  UseMethod("setup_F_sugarbait", mod_name) 
}

#' @title Setup F_sugarbait 
#' 
#' @description Setup a static model for  
#' availability of sugarbaits 
#' 
#' @inheritParams setup_F_sugarbait
#' 
#' @return an **`xds`** model object
#' @export
setup_F_sugarbait.static = function(mod_name, xds_obj, s, options){
  F_sugarbait <- Zero_tV 
  F_sugarbait$get_variables <- get_N_sugarbaits 
  xds_obj$variables$sugarbait_obj$sugarbait_obj$F_sugarbait = F_sugarbait 
  return(xds_obj) 
}

#' @title Setup sugarbait Availability Function 
#' 
#' @description Setup a function to model 
#' dynamic availability of sugarbait 
#' 
#' @inheritParams setup_F_sugarbait
#' 
#' @return an **`xds`** model object
#' @export
setup_F_sugarbait.ts_func = function(mod_name, xds_obj, s, options){
  class(xds_obj$variables$sugarbait_obj$sugarbait_obj) = 'dynamic'
  class(xds_obj$variables$sugarbait_obj) = 'dynamic'
  class(xds_obj$beta) = 'dynamic'
  xds_obj$variables$sugarbait_obj$sugarbait_obj[[s]]$F_sugarbait= make_ts_function(options) 
  return(xds_obj) 
}

