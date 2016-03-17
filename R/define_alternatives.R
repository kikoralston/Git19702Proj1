library(miscTools)
source("EngEcon.R")

compute.congestion.cost.minute <- function(congestion.costs) {
  # Computes congestion cost per minute using commuter data
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
  # Computes annual mortality cost

  annual.cost.mortality <- parameters$vsl * sum(transit.risks$victims$fatality)
  cost.mortality <- rep(annual.cost.mortality, parameters$num.years)
  return(cost.mortality)
}

compute.injury.costs <- function(parameters, transit.risks, injury.costs){
  # Computes annual injury cost

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
    bus.total.time.annual*cost.per.idle.minute*parameters$share.idle.time

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
    bus.total.time.annual*cost.per.idle.minute*parameters$share.idle.time

  return(rep(annual.ghg.cost, parameters$num.years))
}

compute.AM.capital.costs <- function(parameters, buses.data, costs.am) {
  # Compute capital costs of implementing AM

  # capital costs are only incurred in the first year

  # Summing different costs for each age of bus
  cap.cost.am <- sum(buses.data$number *
    c(costs.am$cc.old.bus, costs.am$cc.old.bus, costs.am$cc.old.bus))

  # Adding cost of simulator facility
  cap.cost.am <- cap.cost.am + costs.am$simulator

  return(c(cap.cost.am, rep(0, (parameters$num.years-1))))
}

compute.AM.oem.costs <- function(parameters, buses.data, costs.am) {
  # Compute OeM costs of AM

  n.years.before.am <- parameters$year.AV - parameters$year.ini

  # oem
  oem.cost.am <- sum(buses.data$number) * costs.am$oemc

  # expected overhaul costs
  n.overhaul.cases <- length(costs.am$overhaul$years)
  overhaul.costs <- rep(0, parameters$num.years)
  for (i in 1:n.overhaul.cases) {
    year.overhaul <- n.years.before.am + costs.am$overhaul$years[i]
    overhaul.costs[year.overhaul] <- overhaul.costs[year.overhaul] +
      costs.am$overhaul$prob[i] * costs.am$overhaul.cost *
      sum(buses.data$number)
  }

  # create vector of OeM costs
  oem.cost.am <- c(rep(0, (n.years.before.am)),
                   rep(oem.cost.am, parameters$num.years - n.years.before.am))

  # include expected overhaul cost
  oem.cost.am <- oem.cost.am + overhaul.costs

  return(oem.cost.am)
}

alternative1 <- function(parameters, buses.data, transit.risks,
                         bus.emission, emission.costs, injury.costs,
                         transit.data, congestion.costs){
  # Creates list with cash flow for each cost for alternative 1

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

alternative2 <- function(parameters, buses.data, transit.risks,
                         bus.emission, emission.costs, injury.costs,
                         transit.data, congestion.costs, costs.am){
  # Creates list with cash flow for each cost for alternative 2

  bc.flow <- list()

  # AM capital costs
  bc.flow$cost.AM.capital <- compute.AM.capital.costs(parameters,
                                                      buses.data,
                                                      costs.am)

  # AM OeM costs
  bc.flow$cost.AM.oem <- compute.AM.oem.costs(parameters,
                                              buses.data,
                                              costs.am)

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

exp.alternative2 <- function(parameters, buses.data, transit.risks,
                             bus.emission, emission.costs, injury.costs,
                             transit.data, congestion.costs, costs.am,
                             change.commute){
  # computes expected cash flow for alternative 2
  # takes into account uncertainty in congestion and safety

  # reads data of fatalities and injury change
  fatality.change <- read.csv(file= "../csvfiles/fatalityreduction.csv")
  injury.change <- read.csv(file= "../csvfiles/injuriesreduction.csv")

    # computes median of all experts
  fatality.change.mean <- colMedian(fatality.change)
  injury.change.mean <- colMedian(injury.change)

  # probabilities of each case (guessed by us)
  prob.safety <- c(0.1, 0.8, 0.1)

  # computes \sum_i \sum_j CashFlow_{i,j} * Prob_{i,j}
  # assumes SAFETY and CONGESTION are independents
  for (i in 1:3) {
    # compute cash flow for each case of change in CONGESTION

    # change travel time
    transit.data.am <- transit.data
    transit.data.am$Average.time.in.daily.person.trip.minutes <-
      transit.data.am$Average.time.in.daily.person.trip.minutes +
      change.commute$time[i]/60

    for (j in 1:3) {
      # compute cash flow for each case of change in SAFETY
      # (we assume that fatality and injury are the same event)

      # change transit risks
      transit.risks.am <- transit.risks
      transit.risks.am$victims$fatality <- transit.risks.am$victims$fatality*
        (1-fatality.change.mean[j])
      transit.risks.am$victims$injuries <- transit.risks.am$victims$injuries*
        (1-injury.change.mean[j])

      cf.case <- alternative2(parameters, buses.data, transit.risks.am,
                              bus.emission, emission.costs, injury.costs,
                              transit.data.am, congestion.costs, costs.am)

      if (i == 1 && j == 1) {
        # first case: initialize list with cf*probability
        p <- prob.safety[j]*change.commute$prob[i]
        cf.final <- mapply("*", cf.case , list(p), SIMPLIFY = FALSE)
      } else{
        # other cases: multiply by probability and sum with previous cases
        p <- prob.safety[j]*change.commute$prob[i]
        cf.final <- mapply(function(x, y, wx, wy) wx*x+wy*y,
                           cf.final, cf.case,
                           MoreArgs = list(wx=1, wy=p), SIMPLIFY = FALSE)

      }
    }
  }

  # returns final expected cash flow
  return(cf.final)
}
