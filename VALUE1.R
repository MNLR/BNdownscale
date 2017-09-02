library(R.VALUE)
library(bnlearn)
library(gRain)
library(igraph)
library(shape)
library(flexclust)
library(transformeR)
library(parallel)

source("functions/downscaling/build.downscalingBN.R")
source("functions/downscaling/downscale.BN.R")
source("functions/local.bnlearning/hc.local2.R")
source("functions/local.bnlearning/tabu.local2.R")
source("functions/plot.graph.functions/plot.DBN.R")

####
####   DATA 
####

load("data/era_interim/predictors_germany.Rdata")

globalMM <- makeMultiGrid(q850.germany, t850.germany, z850.germany)
global <- list(q850.germany, t850.germany, z850.germany)

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1


####
####
####
# Test for the 3 modes
DBN.m3.d3.k12 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                    bnlearning.args.list = list(distance =3) )

DBN.m1.d2.k12 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           bnlearning.args.list = list(distance =2) )

DBN.m2.d3.k12 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) ,
                                     param.learning.method = "bayes")
plot.DBN( DBN.m2.d3.k12, dev=TRUE , nodes = c(8, 17))

DBN.m3.dINF.k12 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ) )
plot.DBN( DBN.m3.dINF.k12, dev=TRUE )

# Using MultiGridS

MMDBN.m3.d3.k12 <- build.downscalingBN(local, globalMM , mode = 3, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) )

MMDBN.m2.d2.k12 <- build.downscalingBN(local, globalMM , mode = 2, bnlearning.algorithm = hc.local2, 
                                       clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                       bnlearning.args.list = list(distance =2) )

MMDBN.m1.d25.k12 <- build.downscalingBN(local, globalMM , mode = 1, bnlearning.algorithm = hc.local2, 
                                       clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                       bnlearning.args.list = list(distance =2.5) )
plot.DBN( MMDBN.m3.d3.k12, dev=TRUE , nodes = c(8, 17))
plot.DBN( MMDBN.m2.d2.k12, dev=TRUE , nodes = c(8, 17))
plot.DBN( MMDBN.m1.d25.k12, dev=TRUE , nodes = c(8, 17))



# Tests

DBN.m3.d3.k2 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance =3) , param.learning.method = "mle")
DBN.m3.d3.k2$BN.fit$D.4882

DBN.m3.d3.k4 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 4, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance =3) , param.learning.method = "bayes")
DBN.m3.d3.k4$BN.fit$D.4882

# prediction test
#####  

#test <- list(subsetGrid(q850.germany, years = 1990), subsetGrid(t850.germany, years = 1990), subsetGrid(z850.germany, years = 1990))
MMtest <- subsetGrid(globalMM, years = 1990, season = c(1) )
real <- subsetGrid(local, years = 1990, season = c(1) )


# Testing compatibility...
# this BN was built using global as list:
DBN.m1.d2.k4 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 4, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 2) , param.learning.method = "bayes")
# predictors are given as a Multigrid:
d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) 

# testing predictions...
DBN.m3.d2.k12.B <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 2) , param.learning.method = "bayes")

d.bn<- downscale.BN(DBN.m3.d2.k12.B , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) 

colnames(d.bn[1, ,])[1]

DBN.m2.d25.k12 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 2.5) , param.learning.method = "bayes")
plot.DBN( DBN.m2.d25.k12, dev=TRUE , nodes = c( 23 ))

#      d.bn <- downscale.BN(DBN.m1.d25.k12 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) ## revienta, demasiados arcos, paralelizado

d.bn <- downscale.BN(DBN.m2.d25.k12 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) 
threshold <- marginals(DBN.m2.d25.k12)

DBN.m2.d3.k48 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                    clustering.args.list = list(k = 48, family = kccaFamily("kmeans") ), 
                                    bnlearning.args.list = list(distance = 3) , param.learning.method = "bayes")
plot.DBN( DBN.m2.d3.k48, dev=TRUE , nodes = c( 23 ))

d.bn <- downscale.BN(DBN.m2.d3.k48 , MMtest , parallelize = FALSE , as.matrix = TRUE) # con parallelize casca
threshold <- marginals(DBN.m2.d3.k48)
is.mostLikely(d.bn, event = "1", threshold.vector = threshold["1",])
convert.toMostLikely(d.bn)
real$Data

##

DBN.m1.d3.k24 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 24, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance = 3) , param.learning.method = "bayes")
plot.DBN( DBN.m1.d3.k24, dev=TRUE , nodes = c(3,4,5 ))

memory.size(max = TRUE)

d.bn <- downscale.BN(DBN.m1.d3.k24 , MMtest , parallelize = TRUE , as.matrix = TRUE) # casca
threshold <- marginals(DBN.m1.d3.k24)
is.mostLikely(d.bn, event = "1", threshold.vector = threshold["1",])
convert.toMostLikely(d.bn)
real$Data

DBN.m3.d6.k48 <- build.downscalingBN(local, global , mode = 3, bnlearning.algorithm = hc.local2, 
                                     clustering.args.list = list(k = 48, family = kccaFamily("kmeans") ), 
                                     bnlearning.args.list = list(distance = 6) , param.learning.method = "bayes")
d.bn <- downscale.BN(DBN.m3.d6.k48 , MMtest , parallelize = TRUE , as.matrix = TRUE) # casca
plot.DBN( DBN.m3.d6.k48, dev=TRUE , nodes = c(3,4,5 ))



# time
system.time( d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest , parallelize = TRUE , as.matrix = TRUE,  n.cores = 4) )
#user  system elapsed 
#9.78   32.79  163.28 
system.time(  d.bn<- downscale.BN(DBN.m1.d2.k4 , MMtest ) )
#   user  system elapsed 
#267.44   27.68  297.12 



I.DBN <- build.downscalingBN(local, global , mode = 1, output.marginals = TRUE, bnlearning.algorithm = hc.local2, 
                                      clustering.args.list = list(k = 24, family = kccaFamily("kmeans") ),
                                      bnlearning.args.list = list(distance = 3))
plot.DBN( I.DBN, dev=TRUE , nodes = seq(1,40,4))

test <- subsetGrid(globalMM, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN , test , parallelize = FALSE  , as.matrix = TRUE) 
threshold <- I.DBN$marginals
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = 1 - threshold["1",])

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))

