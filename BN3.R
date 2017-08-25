library(transformeR)
library(loadeR.ECOMS)
library(bnlearn)
library(igraph)
library(shape)
library(microbenchmark)

source("plot.graph.functions.R")
source("preprocess.R")
source("tabu.local2.R")
source("learning.complement2.R")
source("build.distanceBlacklist.R")


data("EOBS_Iberia_tp")
data("VALUE_Iberia_tp")

data <- preprocess(EOBS_Iberia_tp, VALUE_Iberia_tp, rm.na = TRUE)

data[[1]][data[[1]] < 1] <-  0
data[[1]][data[[1]] >= 1] <-  1

learnt.tabu2.tp.1 <- tabu.local2(x = data[[1]], positions = data[[2]], distance = 1, plotrestrictions = F)
plot.restrictedgraph(learnt.tabu2.tp.1, positions = data[[2]], distance = 1 , node = c(1,2,3,4,5,6,7,8,9,10), dev=TRUE )