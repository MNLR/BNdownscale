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
BNB.args.list <- list(categorization.type = "nodeSimple",
        forbid.global.arcs = TRUE,
        forbid.local.arcs = FALSE,
        bnlearning.algorithm = "gs", 
        ncategories = 4,
        clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
        parallelize = TRUE, n.cores = 7,
        output.marginals = TRUE, 
        #bnlearning.args.list = list(distance = 3),
        bnlearning.args.list = list(test = "mi-sh"),
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
                    plot_.DBN = plot_.DBN, plot.aucS = plot.aucS, plot.MI = plot.MI,
                    reference = reference, only.loes.third = only.loes.third)
res2 <- list(results = results, args = BNB.args.list)
save(res2, file="CV/GER186.RData") 

#$AUCS
#[1] 0.7657427 0.7987947 0.7854593 0.7782032 0.7727147 0.7844675 0.7679915 0.7872363 0.7609635 0.7872455 0.7699813 0.7376361 0.7940740
#[14] 0.7985625 0.7579827 0.7859934 0.7736460 0.7891999 0.7990885 0.8017905 0.7650071 0.7698180 0.7849998 0.7739519 0.7823167 0.7626832
#[27] 0.7767249 0.7916826 0.7659419 0.7651433 0.7915712 0.7749554 0.7973046 0.7785403 0.7833913 0.7719246 0.7633782 0.7280815 0.7457558
#[40] 0.7625403 0.7765290 0.7268217 0.7914822 0.7513297 0.7553923 0.7001925 0.7251175 0.7920795 0.7602033 0.7441608 0.7840808 0.7770085
#[53] 0.7904603

#$FRatio
#0.8039913 

#$d.bias
#$d.bias$annual
#[1] 123.2932

#$d.bias$DJF
#[1] 132.6977

#$d.bias$MAM
#[1] 136.8391

#$d.bias$JJA
#[1] 125.5365

#$d.bias$SON
#[1] 174.5432

