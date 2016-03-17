library(tidyr)

source("EngEcon.R")
source("define_alternatives.R")
source("conditionalProb.R")

# -------------------------------------------------
# Creates lists of parameters, costs and other inputs

# **parameters**
parameters <- list()
parameters$disc.rate <- 0.05 # discount rate (% p.y.)
parameters$num.years <- 30 # number of years of analysis
parameters$year.ini <- 2016 # initial year of cash flow
parameters$year.AV <- 2020 # initial year of AV implementation
parameters$ny.population <- 8.4e6 # population
parameters$vsl <- 9.2e6 # value of statistical life ($)
# share of time that vehicle is idle (our assumption)
parameters$share.idle.time <- 0.1

# **injury costs**
# source: https://wisqars.cdc.gov:8443/costT/cost_Part1_Intro.jsp
injury.costs <- list()
injury.costs$occupant <- 173711
injury.costs$motorciclyst <- 198668
injury.costs$pedalciclyst <- 187807
injury.costs$pedestrian <- 191813
injury.costs$other <- 139217

# **number of buses in NYC segmented by age (source: memo)**
buses.data <- data.frame(age = c(4, 9, 20),
                         number = c(2313, 1296, 1437))

# **costs of automerge (source: memo)**
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

# **Changes in commute times with EM (source: memo)**
change.commute <- data.frame(outcome = c("MB", "LB", "LW"),
                             time = c(-235, -55, 30),
                             margin.error = c(20, 15, 10),
                             prob = c(0.2, 0.4, 0.4))

# **transit risks (source:memo)**
transit.risks <- list()
transit.risks$daily.trips <- 1.52e6
transit.risks$trip.avg.time <- 49
transit.risks$annual.vehicle.miles <- 700e6
transit.risks$annual.passenger.travel <- 926259e3
transit.risks$victims <- data.frame(type = c("motorist", "passenger",
                                             "cyclist", "pedestrian"),
                                    fatality = c(3, 1.5, 0.5, 11),
                                    injuries = c(446, 837, 67, 390))

# **bus gas emissions**
# (source:  Average In-Use Emissions from Urban Buses and School Buses)
bus.emission <- list()
bus.emission$nox <- 17.2 # NOx emission (g/mi)
bus.emission$pm <- 0.172 # particulate emission (g/mi)
bus.emission$hc <- 0.387 # HC emission (g/mi)
bus.emission$co <- 1.118 # CO emission (g/mi)
bus.emission$ghg <- 3659 # GHG emission (g/mi)

bus.emission$nox.idle <- 1.109 # NOx idle emission (g/min)
bus.emission$pm.idle <- 0.04 # particulate idle emission (g/min)
bus.emission$hc.idle <- 0.046 # HC idle emission (g/min)
bus.emission$co.idle <- 0.624 # CO idle emission (g/min)
bus.emission$ghg.idle <- 0 # GHG idle emission (g/min)

# read csv file from EASIUR
easiur <- read.csv(file= "../csvfiles/easiur_nyclocation.csv")
easiur.annual <- easiur[ ,grep("Annual.Ground", names(easiur))]

# **emission costs**
# sources:  http://www3.epa.gov/climatechange/EPAactivities/economics/scc.html
#           http://barney.ce.cmu.edu/~jinhyok/easiur/online/
emission.costs <- list()
# social cost of carbon ($/ton)
emission.costs$scc <- 30
# social cost of PM ($/ton)
emission.costs$pm.2.5 <- easiur.annual$PM25.Annual.Ground
# social cost of SO2 ($/ton)
emission.costs$so2 <- easiur.annual$SO2.Annual.Ground
# social cost of NOx ($/ton)
emission.costs$nox <- easiur.annual$NOX.Annual.Ground
# social cost of NH3 ($/ton)
emission.costs$nh3 <- easiur.annual$NH3.Annual.Ground

# **congestion costs (source: page 8 in memo)**
congestion.costs <- list()
congestion.costs$annual.hours.per.commuter <- 38 # hours
congestion.costs$annual.cost.per.commuter <- 818 # ($)
congestion.costs$cost.per.hour <- 22 # $/hour for each commuter

# **NYC transit data**
transit.data <- read.csv(file= "../csvfiles/urbantranspsystem.csv")

# weather data
weather.data <- read.NYC.weather()

# -------------------------------------------------
# Analysis

order.levels <- c("cost.AM.capital", "cost.AM.oem", "cost.mortality",
                  "cost.injury", "cost.congestion", "cost.air.pollution",
                  "cost.ghg" )

# option 1 (do nothing)
cf.option1 <- alternative1(parameters, buses.data, transit.risks,
                           bus.emission, emission.costs, injury.costs,
                           transit.data, congestion.costs)

npv.costs1 <- as.data.frame(lapply(cf.option1, FUN = npv,
                                  rate = parameters$disc.rate))/1e9
npv.costs1 <- gather(npv.costs1, type, value)
npv.costs1$option <- rep(c("alternative 1"), nrow(npv.costs1))
npv.costs1$type <- factor(npv.costs1$type, levels=order.levels)

# option 2 (perform test)
cf.option2 <- exp.alternative2(parameters, buses.data, transit.risks,
                               bus.emission, emission.costs, injury.costs,
                               transit.data, congestion.costs, costs.am,
                               change.commute, weather.data)

npv.costs2 <- as.data.frame(lapply(cf.option2, FUN = npv,
                                   rate = parameters$disc.rate))/1e9
npv.costs2 <- gather(npv.costs2, type, value)
npv.costs2$option <- rep(c("alternative 2"), nrow(npv.costs2))
npv.costs2$type <- factor(npv.costs2$type, levels=order.levels)

# option 3 (implement AV without testing)
#probs for test alternative

bayesProbs <- compute.conditional.probs(change.commute)
# plots stacked bar plot with results

npv.costs <- rbind(npv.costs1, npv.costs2)

g <- ggplot() + geom_bar(data = npv.costs,
                         aes(x=option, y=value, fill=type),
                         stat = "identity", width = 0.2) +
  theme_bw(base_size = 16) + ylab("NPV ($ Billion)") +
  theme(axis.title.x = element_blank())+
        # legend.position=c(1,1), legend.justification=c(1,1)) +
  guides(fill=guide_legend(title=NULL, reverse = TRUE)) +
  scale_fill_grey() + geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(0, 32, by=2)) +
  coord_cartesian(ylim = c(0, 32))

# width and height are in pixels
png("barplot1.png", width=480, height = 480)
print(g)
dev.off()

# -------------------------------------------------
# Sensitivity Analysis
