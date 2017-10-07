library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)
library(sfsmisc)
library(abind)

source("functions/validation/kfold.BN.R")

# FOLDS AND ARG LIST
year.folds.list <- list(seq(1979, 1984), 
                        seq(1985, 1990), 
                        seq(1991, 1996), 
                        seq(1997, 2002), 
                        seq(2003, 2008))

plot.aucS <- TRUE
plot.MI <- TRUE
mi.threshold <- 0.35
plot_.DBN <- TRUE
reference <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip")
only.loes.third <- TRUE
BNB.args.list <- list(categorization.type = "varsSimple",
                      forbid.global.arcs = TRUE,
                      forbid.local.arcs = FALSE,
                      bnlearning.algorithm = "hc", 
                      ncategories = 4,
                      parallelize = TRUE, n.cores = 7,
                      output.marginals = TRUE, 
                      param.learning.method = "bayes")

###
### DATA
###

load("data/era_interim/predictors_germany.Rdata")
global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
local$Data[ local$Data < 1  ] <-  0
local$Data[ local$Data >= 1 ] <-  1
BNB.args.list[["global"]] <- global
BNB.args.list[[ "local" ]] <- local

results <- kfold.BN(year.folds.list = year.folds.list, mi.threshold = mi.threshold, BNB.args.list = BNB.args.list, 
                    plot_.DBN = plot_.DBN, plot.aucS = plot.aucS, plot.MI = plot.MI,
                    reference = reference, only.loes.third = only.loes.third)

res51 <- list(results = results, args = BNB.args.list)
save(res51, file="CV/varsSimple.hc.fg.4.RData") 