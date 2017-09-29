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
source("functions/local.bnlearning/hc.local.R")
source("functions/local.bnlearning/tabu.local.R")
source("functions/local.bnlearning/gs.local.R")
source("functions/local.bnlearning/iamb.local.R")
source("functions/local.bnlearning/fast.iamb.local.R")
source("functions/local.bnlearning/inter.iamb.local.R")
source("functions/local.bnlearning/mmpc.local.R")
source("functions/local.bnlearning/si.hilton.pc.local.R")
source("functions/plot.graph.functions/plot.DBN.R")
source("functions/downscaling/marginals.R")
source("functions/validation/c.table.R")
source("functions/validation/auc.DBN.R")
source("functions/validation/MI.vs.distance.R")
source("functions/downscaling/aux_functions/is.mostLikely.R")
source("functions/validation/c.table.rates.R")

####
####   DATA 
####

load("data/era_interim/predictors_iberia.Rdata")

global <- makeMultiGrid(q850.iberia, t850.iberia, z850.iberia)

local <- VALUE_Iberia_tp
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1

global <- getTemporalIntersection(obs = local, prd = global, which.return = "prd")

DBN <- build.downscalingBN(local, global, mode = 30, bnlearning.algorithm = "hc.local", 
                           ncategories = 4,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           #bnlearning.args.list = list(distance = 6, alpha = 0.01, debug = TRUE),
                           bnlearning.args.list = list(distance = 5),
                           param.learning.method = "bayes")

plot.DBN( DBN, dev=TRUE  , edge.arrow.size = 0.25, node.size = 0)
arc.strength(DBN$BN, DBN$training.data, criterion = "mi")

test <- subsetGrid(global, years = c(1991, 1992, 1993) )
real <- subsetGrid(local, years =  c(1991, 1992, 1993)  )

test <- global
real <- local

downscaled <- downscale.BN(DBN , test , parallelize = TRUE ,  n.cores = 7, prediction.type = "probabilities")
MPT <- DBN$marginals
P_1 <- MPT["1", ]


prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1 - P_1)
ct <- c.table(prediction, real$Data)
ct
c.table.rates(ct, "all")

###
###
###   AUCs
###


aucS <- auc.DBN(downscaled = downscaled, 
                realData = real$Data, 
                plot.curves = TRUE, 
                points = 100)
aucS
c(mean(aucS), min(aucS), max(aucS))


ct1 <- c.table(prediction[ , 1], real$Data[ , 1]) # First station AUC ~~ 0.89
ct1
c.table.rates(ct1, "all")

###
###    Mutual Information
###
###

a <- MI.vs.distance(local, season = "DJF")
mia <- data.frame(y = a[ 2, ], x = a[ 1, ])
mial <- lm( y ~ x , mia )

dev.new()
plot(a[1, ], a[ 2, ], xlab = "Distance", ylab = "Mutual Information")
abline(mial)

# Predictions:

prediction.p <- real
prediction.p$Data <- prediction
attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions
attr(prediction.p$Data, 'season') <- attributes(real$Data)$season


c <- MI.vs.distance(prediction.p)
mic <- data.frame(y = c[ 2, ], x = c[ 1, ])
micl <- lm( y ~ x , mic )

plot(c[1, ], c[ 2, ], col =  "red")
abline(micl, col = "red")
