
compute.conditional.probs <- function(change.commute) {
  # function to compute conditional probabilities using bayes rule
  
  # save priors in new variables (to make code clearer)
  p.MB <- change.commute$prob[change.commute$outcome == "MB"]
  p.LB <- change.commute$prob[change.commute$outcome == "LB"]
  p.LW <- change.commute$prob[change.commute$outcome == "LW"]
  
  # read conditional probabilities of t (result of test) given the true outcome
  # (MB, LB, LW)
  p.t.given.LB <- read.csv(file= "../csvfiles/probabilityLB.csv")
  p.t.given.LW <- read.csv(file= "../csvfiles/probabilityLW.csv")
  p.t.given.MB <- read.csv(file= "../csvfiles/probabilityMB.csv")
  
  # change names of columns
  names(p.t.given.MB) <- c("size","tMB", "tLB", "tLW")
  names(p.t.given.LB) <- c("size","tMB", "tLB", "tLW")
  names(p.t.given.LW) <- c("size","tMB", "tLB", "tLW")
  
  # for the size of test defined above, compute marginal probabilities
  # of each test result (Total probability)
  
  # P(t == MB)
  p.tMB <- p.t.given.MB$tMB*p.MB + p.t.given.LB$tMB*p.LB + p.t.given.LW$tM*p.LW
  # P(t == LB)
  p.tLB <- p.t.given.MB$tLB*p.MB + p.t.given.LB$tLB*p.LB + p.t.given.LW$tLB*p.LW
  # P(t == LW)
  p.tLW <- p.t.given.MB$tLW*p.MB + p.t.given.LB$tLW*p.LB + p.t.given.LW$tLW*p.LW
  
  # creates data frame with marginal probabilities for tests
  p.t <- data.frame(size = p.t.given.MB$size, 
                    tMB = p.tMB, tLB = p.tLB, tLW = p.tLW) 
  
  # compute conditional probabilities P(real outcome | test)
  # P(real outcome | test)  = P(real outcome , test) / P(test)
  #                         = P(test | real outcome) * P(real outcome) / P(test)
  
  # allocate data frames with resulting conditional probabilities
  p.real.given.tMB <- data.frame(size=p.t.given.MB$size,
                                 MB = rep(0, nrow(p.t.given.MB)),
                                 LB = rep(0, nrow(p.t.given.MB)),
                                 LW = rep(0, nrow(p.t.given.MB))) 
  p.real.given.tLB <- data.frame(size=p.t.given.MB$size,
                                 MB = rep(0, nrow(p.t.given.MB)),
                                 LB = rep(0, nrow(p.t.given.MB)),
                                 LW = rep(0, nrow(p.t.given.MB))) 
  p.real.given.tLW <- data.frame(size=p.t.given.MB$size,
                                 MB = rep(0, nrow(p.t.given.MB)),
                                 LB = rep(0, nrow(p.t.given.MB)),
                                 LW = rep(0, nrow(p.t.given.MB))) 
  
  # computes conditional probabilities
  
  # P(real outcome | test == MB)
  p.real.given.tMB$MB <- p.t.given.MB$tMB * p.MB / p.tMB
  p.real.given.tMB$LB <- p.t.given.LB$tMB * p.LB / p.tMB
  p.real.given.tMB$LW <- p.t.given.LW$tMB * p.LW / p.tMB

  # P(real outcome | test == LB)
  p.real.given.tLB$MB <- p.t.given.MB$tLB * p.MB / p.tLB
  p.real.given.tLB$LB <- p.t.given.LB$tLB * p.LB / p.tLB
  p.real.given.tLB$LW <- p.t.given.LW$tLB * p.LW / p.tLB

  # P(real outcome | test == LW)
  p.real.given.tLW$MB <- p.t.given.MB$tLW * p.MB / p.tLW
  p.real.given.tLW$LB <- p.t.given.LB$tLW * p.LB / p.tLW
  p.real.given.tLW$LW <- p.t.given.LW$tLW * p.LW / p.tLW
  
  # creates list with all conditional probability curves
  list.out <- list(tMB=p.real.given.tMB, 
                   tLB=p.real.given.tLB,
                   tLW=p.real.given.tLW)

  # returns list
  return(list.out)
}