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

test <- subsetGrid(global, years = c(1991), season = c(2))
real <- subsetGrid(local,  years = c(1991), season = c(2))

DBN <- build.downscalingBN(local, global, categorization.type = "nodesEven",
                           forbid.global.arcs = TRUE,
                           forbid.local.arcs = FALSE,
                           bnlearning.algorithm = "gs", 
                           ncategories = 4,
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           bnlearning.args.list = list(test = "mc-mi"),
                           param.learning.method = "bayes",
                           two.step = TRUE,
                           return.first = TRUE,
                           bnlearning.algorithm2 = "hc.local",
                           bnlearning.args.list2 = list(distance = 3)
                          )

plot.DBN( DBN, dev=TRUE , edge.arrow.size = 0.50, node.size = 0)
score(DBN$BN, DBN$training.data )

test <- global
real <- local
downscaled <- downscale.BN(DBN , test, parallelize = TRUE,  n.cores = 7) 
#dprediction <- downscale.BN(DBN , test, prediction.type = "event", parallelize = TRUE,  n.cores = 7) 

MPT <-DBN$marginals
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = 1- P_1)
ct <- c.table(prediction, real$Data)
ct
rates <- c.table.rates(ct, "all")
rates


###  mode 22, ncat 5, d10, AUC [1] 0.7809686 0.7059353 0.8138946
###  mode 20, k 16, d10, AUC [1] 0.7745503 0.7003792 0.8081117
###  mode 22, ncat 5, d3.5, AUC [1] 0.7872823 0.7211412 0.8178377
###  mode 22, ncat 9, d3.5 AUC [1] 0.7960473 0.7272802 0.8287681

###
#real <- local
aucS <- auc.DBN(downscaled = downscaled, 
        realData = real$Data, 
        plot.curves = TRUE, points = 100)
aucS
c(mean(aucS), min(aucS), max(aucS))

###
### Against REA
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

# Predictions:
prediction.p <- local
prediction.p$Data <- prediction

attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions

d <- MI.vs.distance(prediction.p)
mid <- data.frame(y = d[ 2, ], x = d[ 1, ])
midl <- lm( y ~ x , mid )

# 
a <- MI.vs.distance(local)
b <- MI.vs.distance(REA)
c <- MI.vs.distance(oPred)
mia <- data.frame(y = a[ 2, ], x = a[ 1, ])
mial <- lm( y ~ x , mia )
mib <- data.frame(y = b[ 2, ], x = b[ 1, ])
mibl <- lm( y ~ x , mib )


dev.new()
plot(a[1, ], a[ 2, ], xlab = "Distance", ylab = "Mutual Information")
points(b[1, ], b[2, ], col = "blue")
#points(c[1, ], c[2, ], col = "green")
points(d[1, ], d[2, ], col = "red")

abline(mial)
abline(mibl, col = "blue")
abline(midl, col = "red")
#legend(c("Observations", ))
