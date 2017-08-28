library(R.VALUE)
library(bnlearn)
library(igraph)
library(shape)
library(flexclust)
source("build.downscalingBN.R")
source("preprocess.R")
source("preprocess.forKmeans.R")
source("hc.local2.R")
source("build.distanceBlacklist.R")
source("learning.complement2.R")
source("plot.graph.functions.R")
source("downscale.BN.R")


load("data/era_interim/predictors_germany.Rdata")

global <- list(q850.germany, t850.germany, z850.germany)

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip")
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1


# Test for the 3 modes
DBN.m3.d3.k12 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                    bnlearning.args.list = list(distance =3) )

plot.restrictedgraph(DBN.m3.d3.k12$BN, DBN.m3.d3.k12$positions, distance = 3 , node = c(2), dev=TRUE )


DBN.m1.d2.k12 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance =2) )

plot.restrictedgraph(DBN.m1.d2.k12$BN, DBN.m1.d2.k12$positions, distance = 2 , node = c(2), dev=TRUE )

DBN.m2.d2.k12 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =2) )

plot.restrictedgraph( DBN.m2.d2.k12$BN , DBN.m2.d2.k12$positions, distance = 2 , node = c(2), dev=TRUE )


DBN.m2.d3.k12 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) )

plot.restrictedgraph( DBN.m2.d2.k12$BN , DBN.m2.d3.k12$positions, distance = 3 , node = seq(1,48,10), dev=TRUE )


# testing prediction
#####  
#####   downscale.BN is a WIP
test <- list(subsetGrid(q850.germany, years = 1990), 
             subsetGrid(t850.germany, years = 1990),
             subsetGrid(z850.germany, years = 1990))

downscale.BN(DBN.m2.d3.k12 , test )
downscale.BN(DBN.m1.d2.k12 , test )