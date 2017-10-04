library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)
library(sfsmisc)
source("functions/validation/kfold.BN.R")

# FOLDS AND ARG LIST
year.folds.list <- list(seq(1983, 1986), 
                        seq(1987, 1991), 
                        seq(1992, 1996), 
                        seq(1997, 2002))
plot.aucS <- TRUE
plot.MI <- TRUE
mi.threshold <- 0.3
plot_.DBN <- TRUE
BNB.args.list <- list( categorization.type = "nodeEven",
                       forbid.global.arcs = TRUE,
                       forbid.local.arcs = FALSE,
                       bnlearning.algorithm = "hc.local",
                       ncategories = 4,
                       clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                       parallelize = TRUE, n.cores = 7,
                       output.marginals = TRUE, 
                       bnlearning.args.list = list(distance = 3.5),
                       #bnlearning.args.list = list(test = "mc-mi"),
                       param.learning.method = "bayes",
                       two.step = FALSE,
                       return.first = FALSE, # ignored
                       bnlearning.algorithm2 = "hc.local",
                       bnlearning.args.list2 = list(distance = 3)
                      )

###
### DATA
###

load("data/era_interim/predictors_iberia.Rdata")
global <- makeMultiGrid(q850.iberia, t850.iberia, z850.iberia)
local <- VALUE_Iberia_tp
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1

global <- getTemporalIntersection(obs = local, prd = global, which.return = "prd")

BNB.args.list[["global"]] <- global
BNB.args.list[["local"]] <- local


resultsIB <- results <- kfold.BN(year.folds.list = year.folds.list, mi.threshold = mi.threshold, BNB.args.list = BNB.args.list, 
                                 plot_.DBN = plot_.DBN, plot.aucS = plot.aucS, plot.MI = plot.MI)