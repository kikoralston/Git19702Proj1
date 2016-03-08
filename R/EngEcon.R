# Engineering Finance functions

library(ggplot2)

annuity.factor <- function(rate, n) {
  # Computes the annuity factor
  # $$P/A = \frac{1-(1+rate)^{-n}}{rate}$$
  #
  # Args:
  #   rate: discount rate per period
  #   n: number of periods (e.g. years)
  #
  # Returns:
  #   annuity factor
  
  return((1-(1+rate)^(-years))/rate)
}

npv <- function(cf, rate) {
  # Computes the NPV of a given cash flow using given discount rate.
  # $$NPV = \sum_{t=0}^{n}\frac{cf_t}{(1+rate)^{t}}$$
  #
  # Args:
  #   cf:   a vector with net cash flow per period
  #         (convention: cf starts at period zero)
  #   rate: discount rate per period
  #
  # Returns:
  #   NPV
  n <- length(cf) -1 # cf starts at period zero
  discount.factor <- (1/(1+rate))^(0:n)
  sum.npv <- sum(cf*discount.factor)
  return(sum.npv)
}

tornado.plot <- function(df){
  # Creates a tornado plot using ggplot2 geom_bar
  #
  # Args:
  #   df: data frame with data to be plotted.
  #       df MUST have the following columns:
  #         - input.name: name of input changed
  #         - input.value: value of input
  #         - output.value: value of output
  #         - base.value: original output value for this input
  # Returns:
  #   A ggplot2 object representing the tornado plot
  
  # Error handling
  names.df <- c("input.name", "input.value", "output.value", "base.value")
  if (sum(names.df %in% names(df)) < length(names.df)) {
    stop("Check format and column names of data frame!!")
  }

  types.bar <- factor(c("offset1", "negative1", "positive1", 
                        "offset2", "negative2", "positive2"), 
                      levels = c("offset1", "negative1", "positive1", 
                                 "offset2", "negative2", "positive2"))
  col.pallete <- c("#FFFFFF00", "gray10", "gray55", 
                   "#FFFFFF00", "gray10", "gray55")
  list.names <- unique(df$input.name)
  
  df.2 <- data.frame(input.name = character(),
                     type.bar = factor(),
                     value = numeric())  
  min.labels <- data.frame(input.name = character(),
                          y = numeric(),
                          label = numeric())  
  max.labels <- data.frame(input.name = character(),
                           y = numeric(),
                           label = numeric())  
  
  for (i in 1:length(list.names)){
    df.input <- df[as.character(df$input.name) == list.names[i], ]
    base.value <- df.input$base.value[1]
    if (sum(df.input$base.value != base.value) > 0){
      stop(paste("Incoherent base values for input", list.names[i]))
    }
     min.value <- min(df.input$output.value)
     min.input <- df.input$input.value[which.min(df.input$output.value)]
     max.value <- max(df.input$output.value)
     max.input <- df.input$input.value[which.max(df.input$output.value)]
     
     min.labels <- rbind(min.labels,
                        data.frame(input.name=list.names[i], 
                                   y = min.value, label = min.input))
     max.labels <- rbind(max.labels,
                         data.frame(input.name=list.names[i],
                                    y = max.value, label = max.input))
     
     if (min.value < 0 && max.value < 0){
       # all negative
       df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                      type.bar = types.bar[1],
                                      value = max.value))
       df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                      type.bar = types.bar[3],
                                      value = (base.value - max.value)))
       df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                      type.bar = types.bar[5],
                                      value = (min.value - base.value)))
     }else{
       if (min.value < 0 && base.value < 0){
         # min.value < 0, base.value < 0, max.value > 0 
         df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                        type.bar = types.bar[3],
                                        value = base.value))
         df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                        type.bar = types.bar[5],
                                        value = (min.value - base.value)))
         df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                        type.bar = types.bar[4],
                                        value = -min.value))
         df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                        type.bar = types.bar[6],
                                        value = max.value))
       }else{
         if (min.value < 0 && base.value > 0){
           # min.value < 0, base.value > 0, max.value > 0 
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[2],
                                          value = min.value))
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[4],
                                          value = -min.value))
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[5],
                                          value = base.value))
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[6],
                                          value = (max.value - base.value)))
         }else{
           # all positive
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[1],
                                          value = min.value))
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[2],
                                          value = (base.value - min.value)))
           df.2 <- rbind(df.2, data.frame(input.name = list.names[i],
                                          type.bar = types.bar[3],
                                          value = (max.value - base.value)))
         }
       }
     }
  }
  
  # print(df.2)
  
  g <- ggplot() + 
    geom_bar(data = df.2, aes(x=input.name, y=value, fill=type.bar),
             stat="identity", width = 0.4) +
    scale_fill_manual(values = col.pallete, guide=FALSE)+
    geom_text(data = min.labels, aes(x=input.name, y=y, label=label), 
              hjust=1.2)+
    geom_text(data = max.labels, aes(x=input.name, y=y, label=label), 
              hjust=-0.4)+
    theme_bw() + theme(axis.title.y=element_blank()) + 
    coord_flip()
    
  return(g)
}

df.test <-data.frame(input.name = rep(c("A", "B", "C", "D"), c(2, 2, 2, 2)),
                     input.value = c(10, 20, 5, 10, 15, 7, 50, 100),
                     output.value = c(-20, -10, -15, 10, -25, 15, 10, 20),
                     base.value = c(-15, -15, -5, -5, 10, 10, 15, 15))
