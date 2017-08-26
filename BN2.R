library(transformeR)
library(loadeR.ECOMS)
library(bnlearn)
library(igraph)
library(shape)
library(microbenchmark)

source("plot.graph.functions.R")
source("hc.local.R")
source("hc.local2.R")
source("preprocess.R")
source("build.distanceBlacklist.R")
source("learning.complement2.R")


data("EOBS_Iberia_tp")
data("VALUE_Iberia_tp")

data <- preprocess(EOBS_Iberia_tp, VALUE_Iberia_tp, rm.na = TRUE)

data[[1]][data[[1]] < 1] <-  0
data[[1]][data[[1]] >= 1] <-  1

learnt.tp2.5 <- hc.local2(x = data[[1]], positions = data[[2]], distance = 2.5, plotrestrictions = F)
plot.restrictedgraph(learnt.tp2.5, positions = data[[2]], distance = 2.5 , node = c(1,2,3,4,5,6,7,8,9,10, 11), dev=TRUE )