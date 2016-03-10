source("EngEcon.R")

alternative1 <- function(parameters, buses.data, transit.risks, 
                         bus.emission, emission.costs, injury.costs){
  bc.flow <- list()
  
  # mortality
  annual.cost.mortality <- parameters$vsl * sum(transit.risks$victims$fatality)
  bc.flow$cost.mortality <- rep(annual.cost.mortality, parameters$num.years)
  
  # injuries
  inj.victims <- transit.risks$victims$injuries
  names(inj.victims) <- transit.risks$victims$type
  inj.costs <- unlist(injury.costs)
  inj.costs <- c(inj.costs[1], inj.costs[1], inj.costs[2], inj.costs[4])
  annual.cost.injury <- sum(inj.costs*inj.victims)
  bc.flow$cost.injury <- rep(annual.cost.injury, parameters$num.years)
  
  # Traffic Congestion

  # Air pollutants

  # GHG
  
}