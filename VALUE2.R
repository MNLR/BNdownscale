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

load("data/era_interim/predictors_iberia.Rdata")

global <- makeMultiGrid(q850.iberia, t850.iberia, z850.iberia)

local <- VALUE_Iberia_tp
local$Data[ local$Data < 1] <-  0
local$Data[ local$Data >= 1] <-  1

global <- getTemporalIntersection(obs = local, prd = global, which.return = "prd")

####
####    EJ 1
####

I.DBN.m1.dINF.k2 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc, 
                                     clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ) )
plot.DBN( I.DBN.m1.dINF.k2, dev=TRUE , nodes = c( 2, 3 ))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN.m1.dINF.k2 , test , parallelize = TRUE , as.matrix = TRUE) 
threshold <- marginals(I.DBN.m1.dINF.k2)
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = 1-threshold["1",])

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))
####
####    EJ 2
####

I.DBN.m1.d6.k3 <- build.downscalingBN(local, global , mode = 1, bnlearning.algorithm = hc.local2, output.marginals = TRUE, 
                                        clustering.args.list = list(k = 3, family = kccaFamily("kmeans") ),
                                        bnlearning.args.list = list(distance = 6 ))
plot.DBN( I.DBN.m1.d6.k3, dev=TRUE , nodes = c( 2, 3 ))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN.m1.d6.k3 , test , parallelize = TRUE , as.matrix = TRUE) 
threshold <- I.DBN.m1.d6.k3$marginals
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = 1-threshold["1",])
convert.toMostLikely(d.bn) - real$Data
real$Data

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))

####
####    EJ 3
####

I.DBN.m2.d6.k2 <- build.downscalingBN(local, global , mode = 2, bnlearning.algorithm = hc.local2, 
                                        clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ),
                                      bnlearning.args.list = list(distance = 6))
plot.DBN( I.DBN.m2.d6.k2, dev=TRUE , nodes = c( 2, 3 ,34))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN.m2.d6.k2 , test , parallelize = TRUE , as.matrix = TRUE) 
threshold <- marginals(I.DBN.m2.d6.k2)
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = 1-threshold["1",])
convert.toMostLikely(d.bn) - real$Data
real$Data

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))


####
####    EJ 4
####

I.DBN.m3.d6.k24 <- build.downscalingBN(local, global , mode = 3, output.marginals = TRUE, bnlearning.algorithm = hc.local2, 
                                      clustering.args.list = list(k = 24, family = kccaFamily("kmeans") ),
                                      bnlearning.args.list = list(distance = 6))
plot.DBN( I.DBN.m3.d6.k24, dev=TRUE , nodes = c( 2, 3 , 8))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN.m3.d6.k24 , test , parallelize = TRUE , as.matrix = TRUE) 
threshold <- marginals(I.DBN.m3.d6.k24)
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = threshold["1",])

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))

####
####    EJ 5
####

I.DBN.m2.d5.k2 <- build.downscalingBN(local, global , mode = 2, output.marginals = TRUE, bnlearning.algorithm = hc.local2, 
                                       clustering.args.list = list(k = 2, family = kccaFamily("kmeans") ),
                                       bnlearning.args.list = list(distance = 3))
plot.DBN( I.DBN.m2.d5.k2, dev=TRUE , nodes = seq(1,40,4))

test <- subsetGrid(global, years = 1995, season = c(1) )
real <- subsetGrid(local, years = 1995, season = c(1) )

d.bn <- downscale.BN(I.DBN.m2.d5.k2 , test , parallelize = TRUE , as.matrix = TRUE) 
threshold <- I.DBN.m2.d5.k2$marginals
prediction  <- is.mostLikely(d.bn, event = "1", threshold.vector = 1 - threshold["1",])

ct1 <- table(real$Data -prediction)
ct2 <- table(convert.toMostLikely(d.bn) - real$Data )

print(paste0( "FN=", ct1[1]/sum(ct1)))
print(paste0( "FP=", ct1[3]/sum(ct1)))
print(paste0( "T=", (ct1[1] + ct1[3])/sum(ct1)))

print(paste0( "FN=", ct2[1]/sum(ct2)))
print(paste0( "FP=", ct2[3]/sum(ct2)))
print(paste0( "T=", (ct2[1] + ct2[3])/sum(ct2)))


