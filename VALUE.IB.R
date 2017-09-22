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
source("functions/local.bnlearning/hc.local2.R")
source("functions/local.bnlearning/tabu.local2.R")
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

DBN <- build.downscalingBN(local, global, mode = 22, bnlearning.algorithm = hc.local2, 
                           ncategories = 3,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           #bnlearning.args.list = list(alpha = 0.01, debug = TRUE),
                           bnlearning.args.list = list(distance = 6),
                           param.learning.method = "bayes")

plot.DBN( DBN, dev=TRUE , nodes = c(13))

test <- subsetGrid(global, years = c(1991, 1992, 1993) )
real <- subsetGrid(local, years =  c(1991, 1992, 1993)  )

downscaled <- downscale.BN(DBN , global , parallelize = TRUE , as.matrix = TRUE,  n.cores = 7) 
MPT <- DBN$marginals
P_1 <- MPT["1", ]


#P_1_ord <- P_1[c(5,3,1,2,7,8,11,10,4,9,6)]
#downscaled_ord <- downscaled[, ,c(5,3,1,2,7,8,11,10,4,9,6)]

#P_1_ord <- P_1[c(4,1,3,2,6,8,10,11,5,9,7)]
#downscaled_ord <- downscaled[, ,c(4,1,3,2,6,8,10,11,5,9,7)]


P_1_ord <- P_1[c(6,5,1,2,3,8,11,9,7,10,4)]
downscaled_ord <- downscaled[, ,c(6,5,1,2,3,8,11,9,7,10,4)]

prediction  <- is.mostLikely(downscaled_ord, event = "1", threshold.vector = 1 - P_1_ord)
c.table(prediction[,1], real$Data[,1])
c.table.rates(c.table(prediction[,1], real$Data[,1]), "all")

###
###
###   AUCs
###


aucS <- auc.DBN(downscaled = downscaled_ord, 
                realData = real$Data, 
                plot.curves = TRUE)
aucS



###
###    Mutual Information
###
###

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
