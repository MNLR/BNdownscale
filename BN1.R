library(transformeR)
library(loadeR.ECOMS)
library(bnlearn)
library(igraph)
library(shape)

source("plot.graph.functions.R")
source("hc.local.R")
source("hc.local2.R")
source("preprocess.R")


###################
###################
###################   Ejemplo aprendizaje local presencia/ausencia precipitacion
###################

dat <- NCEP_Iberia_tp 
dat$Data[dat$Data < 1] <-  0
dat$Data[dat$Data >= 1] <-  1

list.tp <- preprocess(dat)

learnt.tp <- hc.local(x = list.tp[[1]], positions = list.tp[[2]], distance = 7, plotrestrictions = T)
plot.restrictedgraph(learnt.tp, positions = list.tp[[2]], distance = 7, node =10)
plot.restrictedgraph(learnt.tp, positions = list.tp[[2]], distance = 7, node =-1)

plot(learnt.tp)