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
source("functions/downscaling/aux_functions/c.table.R")

####
####   DATA 
####

load("data/era_interim/predictors_germany.Rdata")

global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
localPRED <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip"  )

local$Data[ local$Data <= 1] <-  0
local$Data[ local$Data > 1 ] <-  1

test <- subsetGrid(global, years = 1995)
real <- subsetGrid(local,  years = 1995)

#from <- array("G.Atmosphere", 53)
#to <-  c("D.3987", "D.47", "D.2760", "D.2761", "D.4297", "D.51", "D.4472", "D.4669", "D.4079", "D.52", "D.4572", "D.4007", "D.3991", "D.4882", "D.4074", "D.4187", "D.4617",  "D.4954", "D.4014", "D.4776", "D.477", "D.55", "D.4499", "D.4710", "D.42", "D.4652", "D.480", "D.4004", "D.4838", "D.812", "D.4637", "D.475", "D.4284", "D.4218", "D.356", "D.4009", "D.4644", "D.4083",   "D.54", "D.4676",  "D.48", "D.4002", "D.488", "D.2006", "D.472", "D.4015", "D.470", "D.468", "D.3994", "D.58", "D.483", "D.49", "D.469")

DBN <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = FALSE, 
                           clustering.args.list = list(k = 4, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance = 2.5),
                           param.learning.method = "bayes")
plot.DBN( DBN, dev=TRUE , nodes = c(28))
score(DBN$BN, DBN$training.data )

downscaled <- downscale.BN(DBN , test , parallelize = FALSE , as.matrix = TRUE,  n.cores = 7) 
MPT <- marginals(DBN)
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector =  1- P_1)

c.table(prediction, real$Data)

arc.strength(DBN$BN, data = DBN$training.data, criterion = "x2")

