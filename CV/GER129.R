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
mi.threshold <- 0.3
plot_.DBN <- TRUE
BNB.args.list <- list(categorization.type = "nodeSimple",
                      forbid.global.arcs = TRUE,
                      forbid.local.arcs = FALSE,
                      bnlearning.algorithm = "gs", 
                      ncategories = 4,
                      clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                      parallelize = TRUE, n.cores = 7,
                      output.marginals = TRUE, 
                      #bnlearning.args.list = list(distance = 3),
                      bnlearning.args.list = list(test = "mi"),
                      param.learning.method = "bayes",
                      two.step = TRUE,
                      return.first = TRUE,
                      bnlearning.algorithm2 = "hc"
                      #bnlearning.args.list2 = list(distance = 2.5)
)

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
                    plot_.DBN = plot_.DBN, plot.aucS = plot.aucS, plot.MI = plot.MI)

res3 <- list(results = results, args = BNB.args.list)
save(res3, file="CV/GER129.RData") 