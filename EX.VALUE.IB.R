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

DBN <- build.downscalingBN(local, global, categorization.type = "varsEven",
                           forbid.global.arcs = TRUE,
                           forbid.local.arcs = FALSE,
                           bnlearning.algorithm = "gs", 
                           ncategories = 5,
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           bnlearning.args.list = list(test = "mc-mi", debug = TRUE),
                           #bnlearning.args.list = list(distance = 4, debug = TRUE),
                           param.learning.method = "bayes",
                           two.step = TRUE,
                           return.first = TRUE,
                           bnlearning.algorithm2 = "hc.local",
                           bnlearning.args.list2 = list(distance = 3, debug = FALSE) 
                           )
plot.DBN( DBN$first, dev=TRUE  , nodes = c(5,9), edge.arrow.size = 0.25, node.size = 0)

DBN <- DBN$last

plot.DBN( DBN, dev=TRUE  , nodes = c(5,9), edge.arrow.size = 0.25, node.size = 0)

arc.strength(DBN$BN, DBN$training.data, criterion = "mi")

test <- subsetGrid(global, years = c(1991) )
real <- subsetGrid(local, years =  c(1991)  )

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

# 0.8745241 0.8163548 0.8677363 0.8440301 0.8452090 0.7790472 0.8408516 0.7948611 0.8611588 
# 0.7998096 0.8553792
# 0.8344511 0.7790472 0.8745241

ratest <- 3
ct1 <- c.table(prediction[ , ratest], real$Data[ , ratest]) # First station AUC ~~ 0.89
ct1
c.table.rates(ct1, "all")

###
###    Mutual Information
###
###

attr(real$Data, 'dimensions') <- c("time", "station")
dev.new()
distance.bias(real, prediction, season = "DJF",dimFix = TRUE, plot_ = TRUE)


###
###
###

dev.new()
a <- MI.vs.distance(local, season = "DJF")

