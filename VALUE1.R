library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)

source("functions/downscaling/build.downscalingBN.R")
source("functions/downscaling/downscale.BN.R")
source("functions/local.bnlearning/hc.local2.R")
source("functions/local.bnlearning/tabu.local2.R")
source("functions/plot.graph.functions/plot.DBN.R")

####
####   DATA 
####

load("data/era_interim/predictors_germany.Rdata")

globalMM <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
global <- list(q850.germany, t850.germany, z850.germany)

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip")
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1


####
####
####
# Test for the 3 modes
DBN.m3.d3.k12 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                    bnlearning.args.list = list(distance =3) )

DBN.m1.d2.k12 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance =2) )

DBN.m2.d3.k12 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) ,
                                     param.learning.method = "bayes")

plot.DBN( DBN.m2.d3.k12, dev=TRUE , nodes = c(8, 17))

# Using MultiGridS

MMDBN.m3.d3.k12 <- build.downscalingBN(local, globalMM , mode = 3, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) )

MMDBN.m2.d2.k12 <- build.downscalingBN(local, globalMM , mode = 2, bnlearning.algorithm = hc.local2, 
                                       clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                       bnlearning.args.list = list(distance =2) )

MMDBN.m1.d25.k12 <- build.downscalingBN(local, globalMM , mode = 1, bnlearning.algorithm = hc.local2, 
                                       clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                       bnlearning.args.list = list(distance =2.5) )
plot.DBN( MMDBN.m3.d3.k12, dev=TRUE , nodes = c(8, 17))
plot.DBN( MMDBN.m2.d2.k12, dev=TRUE , nodes = c(8, 17))
plot.DBN( MMDBN.m1.d25.k12, dev=TRUE , nodes = c(8, 17))



# Tests

DBN.m3.d3.k2 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance =3) , param.learning.method = "mle")
DBN.m3.d3.k2$BN.fit$D.4882

DBN.m3.d3.k4 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 4, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) , param.learning.method = "bayes")
DBN.m3.d3.k4$BN.fit$D.4882

# prediction test
#####  

#test <- list(subsetGrid(q850.germany, years = 1990), subsetGrid(t850.germany, years = 1990), subsetGrid(z850.germany, years = 1990))
MMtest <- subsetGrid(globalMM, years = 1990, season = c(1) )

# Testing compatibility...
# this BN was built using global as list:
DBN.m1.d2.k4 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 4, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 2) , param.learning.method = "bayes")
# predictors are given as a Multigrid:
d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) 

# testing predictions...
DBN.m3.d2.k12 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 2) , param.learning.method = "bayes")

d.bn<- downscale.BN(DBN.m3.d2.k12 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) 

# time
system.time( d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) )
#user  system elapsed 
#9.78   32.79  163.28 
system.time(  d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest ) )
#   user  system elapsed 
#267.44   27.68  297.12 

