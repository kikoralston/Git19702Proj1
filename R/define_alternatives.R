source("EngEcon.R")

compute.congestion.cost.minute <- function(congestion.costs) {
  # Computes congestion cost per minute from commuter data
  return(congestion.costs$annual.cost.per.commuter/
           (congestion.costs$annual.hours.per.commuter*60))
}

compute.congestion.costs <- function(parameters, congestion.costs, 
                                    transit.data) {
  # Computes annual congestion cost
  
  # computes congestion cost per minute
  cong.cost.minute <- compute.congestion.cost.minute(congestion.costs)
  # computes total time in traffic for road vehicles per year
  total.time.person.trip <- sum(
    transit.data$Daily.Person.Trips.millions[1:3]*1e6*
      transit.data$Average.time.in.daily.person.trip.minutes[1:3])
  annual.congestion.cost <- cong.cost.minute*total.time.person.trip  
  return(rep(annual.congestion.cost, parameters$num.years))
}

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

compute.pollution.costs <- function(parameters, transit.data, bus.emission,
                                    emission.costs){
  # computes air pollution costs (only for buses)

  idx.bus <- which(transit.data$Mode == "Bus")
  
  # total miles travelled by bus
  bus.miles.travel.annual <- 
    transit.data$Annual.vehicle.miles.traveled.100.millions[idx.bus]*100e6
  
  # total time travelled by bus
  bus.total.time.annual <- 
    transit.data$Daily.Person.Trips.millions[idx.bus]*1e6*
    transit.data$Average.time.in.daily.person.trip.minutes[idx.bus]*360
  
  cost.per.mile <- bus.emission$nox*emission.costs$nox/1e6 +
    bus.emission$pm*emission.costs$pm.2.5/1e6
    
  cost.per.idle.minute <- bus.emission$nox.idle*emission.costs$nox/1e6+
    bus.emission$pm.idle*emission.costs$pm.2.5/1e6
  
  # pollution costs = in-use pollution + idle pollution
  # IMPORTANT: I ASSUMED THAT ONLY 10% OF TRAVEL TIME IS IDLE TIME
  # CHECK THIS ASSUMPTION
  annual.pollution.cost <- bus.miles.travel.annual*cost.per.mile+
    bus.total.time.annual*cost.per.idle.minute*0.1
  
  return(rep(annual.pollution.cost, parameters$num.years))
}

compute.ghg.costs <- function(parameters, transit.data, bus.emission,
                              emission.costs){
  # computes GHG costs (only for buses)
  
  idx.bus <- which(transit.data$Mode == "Bus")
  
  # total miles travelled by bus
  bus.miles.travel.annual <- 
    transit.data$Annual.vehicle.miles.traveled.100.millions[idx.bus]*100e6
  
  # total time travelled by bus
  bus.total.time.annual <- 
    transit.data$Daily.Person.Trips.millions[idx.bus]*1e6*
    transit.data$Average.time.in.daily.person.trip.minutes[idx.bus]*360
  
  cost.per.mile <- bus.emission$ghg*emission.costs$scc/1e6
  
  cost.per.idle.minute <- bus.emission$ghg.idle*emission.costs$scc/1e6
  
  # GHG costs = in-use pollution + idle pollution
  # IMPORTANT: I ASSUMED THAT ONLY 10% OF TRAVEL TIME IS IDLE TIME
  # CHECK THIS ASSUMPTION
  annual.ghg.cost <- bus.miles.travel.annual*cost.per.mile+
    bus.total.time.annual*cost.per.idle.minute*0.1
  
  return(rep(annual.ghg.cost, parameters$num.years))
}



alternative1 <- function(parameters, buses.data, transit.risks, 
                         bus.emission, emission.costs, injury.costs,
                         transit.data, congestion.costs){
  bc.flow <- list()
  
  # mortality
  bc.flow$cost.mortality <- compute.mortality.costs(parameters, transit.risks)
  
  # injuries
  bc.flow$cost.injury <- compute.injury.costs(parameters, transit.risks, 
                                                 injury.costs)
  
  # Traffic Congestion
  bc.flow$cost.congestion <- compute.congestion.costs(parameters, 
                                                      congestion.costs, 
                                                      transit.data)
  # Air pollutants
  bc.flow$cost.air.pollution <- compute.pollution.costs(parameters, 
                                                        transit.data, 
                                                        bus.emission,
                                                        emission.costs)

  # GHG
  bc.flow$cost.ghg <- compute.ghg.costs(parameters,transit.data,
                                        bus.emission,emission.costs)
  
  # return list with flow of costs
  return(bc.flow)
}