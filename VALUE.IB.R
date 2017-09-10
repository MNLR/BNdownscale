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

load("data/era_interim/predictors_iberia.Rdata")

global <- makeMultiGrid(q850.iberia, t850.iberia, z850.iberia)

local <- VALUE_Iberia_tp
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1

global <- getTemporalIntersection(obs = local, prd = global, which.return = "prd")


DBN <- build.downscalingBN(local, global , mode = 1 , bnlearning.algorithm = tabu,  
                                        parallelize = TRUE,
                                        #bnlearning.args.list = list(distance = 2.5)  ,
                                        clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ),
                                        param.learning.method = "bayes")
plot.DBN( DBN, dev=TRUE , nodes = c(13))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

downscaled <- downscale.BN(DBN , test , parallelize = TRUE , as.matrix = TRUE,  n.cores = 7) 
MPT <- DBN$marginals
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1 - P_1)
prediction.ml <-convert.toMostLikely(downscaled)
c.table(prediction, real$Data)
c.table(prediction.ml, real$Data)

