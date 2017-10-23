library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)
library(sfsmisc)

source("functions/downscaling/build.downscalingBN.R")
source("functions/downscaling/downscale.BN.R")
source("functions/plot.graph.functions/plot.DBN.R")
source("functions/downscaling/marginals.R")
source("functions/validation/c.table.R")
source("functions/validation/auc.DBN.R")
source("functions/validation/distance.bias.R")
source("functions/downscaling/aux_functions/is.mostLikely.R")
source("functions/validation/c.table.rates.R")

####
####   DATA 
####

load("data/era_interim/predictors_germany.Rdata")
global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
# pca.global <- prinComp(global, v.exp = .99)

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
REA <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip"  )
oPred <- loadValueStations(dataset = "data/VALUE_RACMO011_53_Germany_v1.zip", var = "precip"  )

local$Data[ local$Data < 1  ] <-  0
local$Data[ local$Data >= 1 ] <-  1

oPred$Data[ oPred$Data < 1  ] <-  0
oPred$Data[ oPred$Data >= 1 ] <-  1
ct.oPred <- c.table(oPred$Data, local$Data)
ct.oPred
rates.oPred <- c.table.rates(ct.oPred, "all")
rates.oPred

REA$Data[ REA$Data < 1  ] <-  0
REA$Data[ REA$Data >= 1 ] <-  1
ct.REA <- c.table(REA$Data, local$Data)
ct.REA
rates.REA <- c.table.rates(ct.REA, "all")
rates.REA

( table(REA$Data)["1"]/sum(table(REA$Data)) )/(table(local$Data)["1"]/sum(table(local$Data)))

test <- subsetGrid(global, years = c(1979))
real <- subsetGrid(local,  years = c(1979))

DBN <- build.downscalingBN(local, global, categorization.type = "nodeClustering",
                                               forbid.global.arcs = TRUE,
                                               forbid.local.arcs = FALSE,
                                               bnlearning.algorithm = "gs",
                                               clustering.args.list = list(k = 12, family = kccaFamily("kmeans")),
                                               #bnlearning.args.list = list(distance = 6),
                                               #ncategories = 4, 
                                               two.step = TRUE, 
					                                     return.first = TRUE,
                                               output.marginals = FALSE,
                                               bnlearning.algorithm2 = "hc",
                                               #bnlearning.args.list2 = list(distance = 2.5),
                                               parallelize = TRUE, n.cores = 7)
                           

plot.DBN( DBN$first, dev=TRUE , edge.arrow.size = 0.25, node.size = 0)
DBN <- DBN$last
plot.DBN( DBN, dev=TRUE , edge.arrow.size = 0.25, node.size = 0)

bnlearn::score(DBN$BN, DBN$training.data )

test <- global
real <- local
downscaled <- downscale.BN(DBN , test, parallelize = TRUE,  n.cores = 7) 
#dperdiction <- downscale.BN(DBN , test, prediction.type = "event", parallelize = TRUE,  n.cores = 7) 

MPT <-DBN$marginals
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1 - P_1)
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = array(0.5, 53 ))

ct <- c.table(prediction, real$Data)
ct
rates <- c.table.rates(ct, "all")
rates



###
#real <- local
aucS <- auc.DBN(downscaled = downscaled, 
        realData = real$Data, 
        plot.curves = TRUE, points = 100)
aucS
c(mean(aucS), min(aucS), max(aucS))

###
### Against REA and RAC
###

est <- 46
c.table(prediction[,est], real$Data[,est])
c.table.rates( c.table(prediction[,est], real$Data[,est]), "all")

c.table(REA$Data[,est], real$Data[,est])
c.table.rates( c.table(REA$Data[,est], real$Data[,est]), "all")

c.table(oPred$Data[,est], real$Data[,est])
c.table.rates( c.table(oPred$Data[,est], real$Data[,est]), "all")

###
###
###   Mutual Information
###

## Bias against rea

dev.new()
distance.bias(local, REA$Data, plot_ = TRUE, colpred = "blue", 
              legend = c("Observed", "GCM prediction"), show.title = FALSE)

## Bias against prediction

dev.new()
distance.bias(real, prediction, third = REA, plot_ = TRUE, colpred = "red" )

## Bias prediction vs REA
dev.new()
distance.bias(REA, prediction, plot_ = TRUE, 
              colpred = "red", colreal = "blue", legend = c("GCM prediction", "Predicted") )
