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
ct.oPred <- c.table(oPred$Data, real$Data)
ct.oPred
rates.oPred <- c.table.rates(ct.oPred, "all")
rates.oPred

REA$Data[ REA$Data < 1  ] <-  0
REA$Data[ REA$Data >= 1 ] <-  1
ct.REA <- c.table(REA$Data, real$Data)
ct.REA
rates.REA <- c.table.rates(ct.REA, "all")
rates.REA


testPRED <- subsetGrid(localPRED, years = c(1991), season = c(2))
test <- subsetGrid(global, years = c(1991), season = c(2))
real <- subsetGrid(local,  years = c(1991), season = c(2))

#from <- array("G.Atmosphere", 53)
#to <-  c("D.3987", "D.47", "D.2760", "D.2761", "D.4297", "D.51", "D.4472", "D.4669", "D.4079", "D.52", "D.4572", "D.4007", "D.3991", "D.4882", "D.4074", "D.4187", "D.4617",  "D.4954", "D.4014", "D.4776", "D.477", "D.55", "D.4499", "D.4710", "D.42", "D.4652", "D.480", "D.4004", "D.4838", "D.812", "D.4637", "D.475", "D.4284", "D.4218", "D.356", "D.4009", "D.4644", "D.4083",   "D.54", "D.4676",  "D.48", "D.4002", "D.488", "D.2006", "D.472", "D.4015", "D.470", "D.468", "D.3994", "D.58", "D.483", "D.49", "D.469")

DBN <- build.downscalingBN(local, global, mode = 50, bnlearning.algorithm = "hc.local", 
                           ncategories = 5,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           #clustering.args.list = list(k = 12, family = kccaFamily("kmedians") ), 
                           #bnlearning.args.list = list(test = "mc-mi", debug = TRUE),
                           bnlearning.args.list = list(distance = 3),
                           param.learning.method = "bayes")

plot.DBN( DBN, dev=TRUE , nodes = c(28))
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
points(c[1, ], c[2, ], col = "green")
points(d[1, ], d[2, ], col = "red")

abline(mial)
abline(mibl, col = "blue")


points(c[1, ], c[ 2, ], col =  "red")
abline(micl, col = "red")
legend(c("Observations", ))
