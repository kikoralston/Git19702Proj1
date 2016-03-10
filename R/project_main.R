
source("EngEcon.R")

# -------------------------------------------------
# Creates lists of parameters, costs and other inputs

# parameters
parameters <- list()
parameters$disc.rate <- 0.05 # discount rate (% p.y.)
parameters$num.years <- 30 # number of years of analysis
parameters$ny.population <- 8.4e6 # population
parameters$vsl <- 9.2e6 # value of statistical life ($)

# injury costs. 
# source: https://wisqars.cdc.gov:8443/costT/cost_Part1_Intro.jsp
injury.costs <- list()
injury.costs$occupant <- 173711
injury.costs$motorciclyst <- 198668
injury.costs$pedalciclyst <- 187807
injury.costs$pedestrian <- 191813
injury.costs$other <- 139217

# number of buses in NYC segmented by age (source: memo)
buses.data <- data.frame(age = c(4, 9, 20), 
                         number = c(2313, 1296, 1437))

# costs of automerge (source: memo)
costs.am <- list()
costs.am$cc.new.bus <- 5000 # cost to install in new bus (<= 4)
costs.am$cc.med.bus <- 6500 # cost to install in medium aged bus (5-9)
costs.am$cc.old.bus <- 8500 # cost to install in old bus (10-20)
costs.am$oemc <- 1500 # operation and maintenance costs
# overhaul years and probabilities
costs.am$overhaul <- data.frame(years=c(4, 5, 6), 
                                prob = c(0.3, 0.5, 0.2))
costs.am$overhaul.cost <- 3000 # overhaul cost
costs.am$simulator <- 2.8e6 # simulator facility

# Changes in commute times with EM
change.commute <- data.frame(outcome = c("MB", "LB", "LW"),
                             time = c(-235, -55, 30),
                             margin.error = c(20, 15, 10),
                             prob = c(0.2, 0.4, 0.4))
# transit risks
transit.risks <- list()
transit.risks$daily.trips <- 1.52e6
transit.risks$trip.avg.time <- 49
transit.risks$annual.vehicle.miles <- 700e6
transit.risks$annual.passenger.travel <- 926259e3
transit.risks$victims <- data.frame(type = c("motorist", "passenger", 
                                             "cyclist", "pedestrian"),
                                    fatality = c(3, 1.5, 0.5, 11),
                                    injuries = c(446, 837, 67, 390))

# -------------------------------------------------
# Analysis

# option 1 (do nothing)

# option 2 (perform test)

# option 3 (implement AV without testing)

# -------------------------------------------------
# Sensitivity Analysis
