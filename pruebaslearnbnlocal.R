
## pruebas de learnbn.local
##
##   # comprobar whitelist conflictiva con blacklist
##  


# https://stackoverflow.com/questions/3057341/how-to-use-rs-ellipsis-feature-when-writing-your-own-function?rq=1

library(bnlearn)
library(igraph)
library(shape)

source("plot.graph.functions.R")
source("hc.local.R")
source("hc.local2.R")



positions <- matrix(c(c(2,2), c(3,4)), ncol = 2)
positions
cosa <- hc.local2(x = cars, positions, 1)
cosa

positions <- matrix(c(c(2,3), c(3,3), c(4,5), c(10,10), c(20,35)), ncol = 5, dimnames = list(NULL, colnames(iris)) )
positions
learning.complement(nodes.centre = 1,nodes = colnames(iris), positions = positions, distance = 4, norm_ = "2")

positions <- matrix(c(c(2,3), c(5,10), c(3,4), c(2,2), c(3,4)), ncol = 5)
positions
cosa1 <- hc.local(x = iris, positions, 1, plotrestrictions = TRUE)
plot(cosa1)
cosa1 <- hc.local(x = iris, positions, 4, plotrestrictions = TRUE)
plot(cosa1)

cosa2 <- hc.local2(x = iris, positions, 0.1)
plot(cosa2)

survey <- read.table("survey.txt", header = TRUE)
colnames(survey) #hay que poner 6 distancias
positions <- matrix(c(c(2,2), c(10,12), c(13, 20), c(3,4), c(5,6), c(7, 20)), ncol = 6, dimnames = list(NULL, colnames(survey)))
#positions <- matrix(c(2,3,5,10,15,50), ncol = 6, dimnames = list(NULL, colnames(survey)))   # en R1, p.e. temporal, distancia 2 = distancia p etc,

positions
distance <- 10
learnt.local <- hc.local(survey, positions, distance, plotrestrictions = TRUE)
learnt.local
plot(learnt.local)

plot.restrictedgraph(learnt.local, positions, distance )
plot.restrictedgraph(learnt.local, positions, distance, nodes = 2 )


##############
##############

plot.graphrestrictions2 <- function(nodes, positions, distance ) {
  minx <- min(positions[1 , ])
  maxx <-  max(positions[1 , ])
  miny <- min(positions[2 ,  ])
  maxy <- max(positions[2 ,  ])
  
  plot(x = positions[1, ], y = positions[2, ],  pch = 19, cex = 1, xlim = c(minx ,maxx) , ylim=c(miny,  maxy ), col=c("red", "blue", "green", "yellow", "orange", "purple") )
  labsx <- positions[1, ] + (maxx - minx)/60
  text(x = labsx , y = positions[2,  ] , labels = colnames(nodes),  col = c("red", "blue", "green", "yellow", "orange", "pink"))
  points(positions[1 ,  ] ,  positions[2 ,  ],  pch = 1, cex = distance,  col = c("red", "blue", "green", "yellow", "orange", "pink")) 
}



##############
##############

dat <- NCEP_Iberia_tp$Data
xy <- NCEP_Iberia_tp$xyCoords


?as.data.frame
df.dat <- as.data.frame(dat)
# comprobaciones 
locs <- matrix(seq(1,48), ncol = 8)
obs <- sample(seq(1,1805), 1)
loc <- sample(seq(1,48), 1)
locs
obs
loc
dat[obs,1,4] == df.dat[obs, loc]