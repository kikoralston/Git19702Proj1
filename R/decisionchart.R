library(diagram)

size.nodes <- 0.02

index.node <- c(3,
                15, 21, 
                32, 38, 
                49, 52, 55, 
                57, 60, 62, 63, 64, 65, 66, 67, 68, 69, 70)

decision.nodes <-c(3)
event.nodes <-c(21, 38, 49, 52, 55)

png(filename = paste0("decisiontree.png"),width = 1000, height = 1000)

par(mar = c(1, 1, 1, 1))
openplotmat()
elpos <- coordinates (rep(14, 5), hor = FALSE)

links<-c(3, 15, 
         3, 21,
         15, 57,
         21, 32, 
         21, 38,
         32, 60,
         38, 49, 
         38, 52, 
         38, 55, 
         49, 62, 
         49, 63, 
         49, 64, 
         52, 65, 
         52, 66, 
         52, 67, 
         55, 68, 
         55, 69, 
         55, 70)

names.edges <- c("Alternative 1", 
                 "Alternative 2",
                 "",
                 "Bad Weather\n(8%)",
                 "Good Weather\n(92%)",
                 "",
                 "MB (20%)", "LB (40%)", "LW (40%)",
                 rep(c("sUB (10%)", "sBE (80%)", "sLB (10%)"),3))

pos.text <- c(2,2,2,2,2,2,2,3,2, rep(c(1,3,3),3))

fromto <- matrix(ncol = 2, byrow = TRUE,data = links)

nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr){
  arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ],
                                from = elpos[fromto[i, 1], ],
                                lwd = 1, arr.length = 0)
}
for (ir in 1:length(names.edges)) {
  text(arrpos[ir, 1], arrpos[ir, 2], names.edges[ir], cex=1.2, pos = pos.text[ir])
}


for (i in 1:length(elpos)){
    if(i %in% event.nodes){
      textellipse (elpos[i,], radx=size.nodes, rady=size.nodes, 
                   box.col = "cornsilk", shadow.col = "grey70", 
                   shadow.size = 0, cex = 0.8)
    }else{
      if(i %in% decision.nodes){
        textrect (elpos[i,], radx=size.nodes, rady=size.nodes, 
                  box.col = "cornsilk", shadow.col = "grey70", 
                  shadow.size = 0, cex = 0.8)
      }
    }        
}

x.pos.nodes <- unique(round(unique(elpos[,1]),5))
y.pos <- 0.015
abline(v = x.pos.nodes[1], lty = "dashed", col = "lightgray")
text(x = x.pos.nodes[1], y = y.pos, "Decision", cex = 1.1)

abline(v = x.pos.nodes[2], lty = "dashed", col = "lightgray")
text(x = x.pos.nodes[2], y = y.pos, "Weather Outcome", cex = 1.1)

abline(v = x.pos.nodes[3], lty = "dashed", col = "lightgray")
text(x = x.pos.nodes[3], y = y.pos, "Traffic Outcome", cex = 1.1)

abline(v = x.pos.nodes[4], lty = "dashed", col = "lightgray")
text(x = x.pos.nodes[4], y = y.pos, "Safety Outcome",cex = 1.1)

terminal.nodes <- index.node[index.node > 56]
names.terminal.nodes <- c("PV1", "PV1", "PV2", "PV3", "PV4", "PV5",
                          "PV6", "PV7", "PV8", "PV9", "PV10")

for (i in 1:length(terminal.nodes)) {
  x.pos <-elpos[terminal.nodes[i], 1]
  y.pos <-elpos[terminal.nodes[i], 2]
  text(x = x.pos, y = y.pos, names.terminal.nodes[i], cex = 1.1, pos = 4)
}

dev.off()