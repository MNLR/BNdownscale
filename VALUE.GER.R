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
source("functions/downscaling/aux_functions/c.table.R")
source("functions/downscaling/marginals.R")
source("functions/downscaling/auc.DBN.R")
source("functions/validation/MI.vs.distance.R")


####
####   DATA 
####

load("data/era_interim/predictors_germany.Rdata")

global <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
# pca.global <- prinComp(global, v.exp = .99)


obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
localPRED <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip"  )

local$Data[ is.nan(local$Data) ] <- NA
local$Data[ local$Data < 1  ] <-  "0"
local$Data[ local$Data >= 1 ] <-  "1"

local2 <- loadValueStations(dataset = obs.dataset, var = "precip"  )
local2$Data[ local2$Data < 1  ] <-  0
local2$Data[ local2$Data >= 1 ] <-  1


localPRED$Data[ is.nan(localPRED$Data) ] <- NA
localPRED$Data[ localPRED$Data < 1  ] <-  "0"
localPRED$Data[ localPRED$Data >= 1 ] <-  "1"

testPRED <- subsetGrid(localPRED, years = c(1991, 1992) )
test <- subsetGrid(global, years = c(1991, 1992) )
real <- subsetGrid(local,  years = c(1991,1992) )

#from <- array("G.Atmosphere", 53)
#to <-  c("D.3987", "D.47", "D.2760", "D.2761", "D.4297", "D.51", "D.4472", "D.4669", "D.4079", "D.52", "D.4572", "D.4007", "D.3991", "D.4882", "D.4074", "D.4187", "D.4617",  "D.4954", "D.4014", "D.4776", "D.477", "D.55", "D.4499", "D.4710", "D.42", "D.4652", "D.480", "D.4004", "D.4838", "D.812", "D.4637", "D.475", "D.4284", "D.4218", "D.356", "D.4009", "D.4644", "D.4083",   "D.54", "D.4676",  "D.48", "D.4002", "D.488", "D.2006", "D.472", "D.4015", "D.470", "D.468", "D.3994", "D.58", "D.483", "D.49", "D.469")

DBN <- build.downscalingBN(local2, global, mode = 22, bnlearning.algorithm = hc.local2, 
                           ncategories = 10,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           clustering.args.list = list(k = 24, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance = 10),
                           param.learning.method = "bayes")
plot.DBN( DBN, dev=TRUE , nodes = c(28))
score(DBN$BN, DBN$training.data )

downscaled <- downscale.BN(DBN , test , parallelize = TRUE , as.matrix = TRUE,  n.cores = 7) 
MPT <-DBN$marginals
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector =  1 - P_1)

rates <- c.table.rates(c.table(prediction, real$Data), "all")
rates


###  mode 22, ncat 10, d10 AUC 0.751135
###
###  
###

dev.new()
auc.DBN(downscaled = downscaled, 
        real = real$Data, event = "1", points = 100,
        plot.curve = TRUE, return.YI = TRUE)
abline(h = rates$TPR, col = "red")

prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector = array(0.2132132, NCOL(prediction)))
rates <- c.table.rates(c.table(prediction, real$Data), "all")
rates

###
###
###   Mutual Information
###

a <- MI.vs.distance(local)
b <- MI.vs.distance(localPRED)
mia <- data.frame(y = a[ 2, ], x = a[ 1, ])
mial <- lm( y ~ x , mia )
mib <- data.frame(y = b[ 2, ], x = b[ 1, ])
mibl <- lm( y ~ x , mib )

dev.new()
plot(a[1, ], a[ 2, ], xlab = "Distance", ylab = "Mutual Information")
points(b[1, ], b[2, ], col = "blue")

abline(mial)
abline(mibl, col = "blue")


# Predictions:
test2 <- subsetGrid(global, years = c(1995) )
downscaled <- downscale.BN(DBN , test2 , parallelize = TRUE , as.matrix = TRUE,  n.cores = 7) 
MPT <-DBN$marginals
P_1 <- MPT["1", ]
prediction  <- is.mostLikely(downscaled, event = "1", threshold.vector =  1 - P_1)

prediction[ prediction == 1] <- "1"

prediction.p <- real
prediction.p$Data <- prediction
attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions

c <- MI.vs.distance(prediction.p)
mic <- data.frame(y = c[ 2, ], x = c[ 1, ])
micl <- lm( y ~ x , mic )

points(c[1, ], c[ 2, ], col =  "red")
abline(micl, col = "red")


############################################################################
#####
#####
#####
#####         STRINGS
#####
#####
############################################################################

DBNstrings <- build.downscalingBN(local, global, mode = 22, bnlearning.algorithm = hc.local2, 
                           ncategories = 10,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           clustering.args.list = list(k = 24, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance = 10),
                           param.learning.method = "bayes")
plot.DBN( DBNstrings, dev=TRUE , nodes = c(28))
scoreDBNStrings <- score(DBNstrings$BN, DBNstrings$training.data )
scoreDBNStrings

downscaledStrings <- downscale.BN(DBNstrings , test , parallelize = TRUE , 
                                  as.matrix = TRUE,  n.cores = 7) 
MPT <-DBNstrings$marginals
P_1 <- MPT["1", ]
predictionStrings  <- is.mostLikely(downscaledStrings, event = "1", threshold.vector =  1 - P_1)

ratesStrings <- c.table.rates(c.table(predictionStrings, real$Data), "all")
ratesStrings


###  mode 22, ncat 10, d10 AUC 0.751135
###
###  
###

dev.new()
auc.values.strings <- auc.DBN(downscaled = downscaledStrings, 
        real = real$Data, event = "1", points = 100,
        plot.curve = TRUE, return.YI = TRUE)
abline(h = ratesStrings$TPR, col = "red")

predictionStringsBest  <- is.mostLikely(downscaledStrings, event = "1", threshold.vector = array(auc.values.strings$best.threshold, NCOL(predictionStrings)))
ratesStringsBest <- c.table.rates(c.table(predictionStringsBest, real$Data), "all")
ratesStringsBest

###
###
###   Mutual Information
###

a <- MI.vs.distance(local)
b <- MI.vs.distance(localPRED)
mia <- data.frame(y = a[ 2, ], x = a[ 1, ])
mial <- lm( y ~ x , mia )
mib <- data.frame(y = b[ 2, ], x = b[ 1, ])
mibl <- lm( y ~ x , mib )

dev.new()
plot(a[1, ], a[ 2, ], xlab = "Distance", ylab = "Mutual Information")
points(b[1, ], b[2, ], col = "blue")

abline(mial)
abline(mibl, col = "blue")


# Predictions:

prediction.p <- real
prediction.p$Data <- predictionStringsBest
attr(prediction.p$Data, 'dim') <- attributes(real$Data)$dim
attr(prediction.p$Data, 'dimensions') <- attributes(real$Data)$dimensions

c <- MI.vs.distance(prediction.p)
mic <- data.frame(y = c[ 2, ], x = c[ 1, ])
micl <- lm( y ~ x , mic )

points(c[1, ], c[ 2, ], col =  "red")
abline(micl, col = "red")




###
###
###  COMPROBACIONEs
###
arc.strength(DBN$BN, data = DBN$training.data, criterion = "x2")

c.table(localPRED$Data, local$Data)

table(localPRED$Data)
table(local$Data)

