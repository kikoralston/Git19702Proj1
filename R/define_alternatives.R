source("EngEcon.R")

compute.mortality.costs <- function(parameters, transit.risks){
  annual.cost.mortality <- parameters$vsl * sum(transit.risks$victims$fatality)
  cost.mortality <- rep(annual.cost.mortality, parameters$num.years)
  return(cost.mortality)
}

compute.injury.costs <- function(parameters, transit.risks, injury.costs){
  inj.victims <- transit.risks$victims$injuries
  names(inj.victims) <- transit.risks$victims$type
  inj.costs <- unlist(injury.costs)
  inj.costs <- c(inj.costs[1], inj.costs[1], inj.costs[2], inj.costs[4])
  annual.cost.injury <- sum(inj.costs*inj.victims)
  return(rep(annual.cost.injury, parameters$num.years))
}

alternative1 <- function(parameters, buses.data, transit.risks, 
                         bus.emission, emission.costs, injury.costs){
  bc.flow <- list()
  
  # mortality
  bc.flow$cost.mortality <- compute.mortality.costs(parameters, transit.risks)
  
  # injuries
  bc.flow$cost.injury <- compute.mortality.costs(parameters, transit.risks, 
                                                 injury.costs)
  
  # Traffic Congestion

  # Air pollutants

  # GHG
  
}