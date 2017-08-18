library(transformeR)
library(loadeR.ECOMS)
library(bnlearn)
library(igraph)
library(shape)
library(microbenchmark)

source("plot.graph.functions.R")
source("hc.local.R")
source("hc.local2.R")
source("preprocess.R")


###################
###################
###################   Ejemplo aprendizaje local presencia/ausencia precipitacion
###################

dat <- NCEP_Iberia_tp 
dat$Data[dat$Data < 1] <-  0
dat$Data[dat$Data >= 1] <-  1

list.tp <- preprocess(dat)

learnt.tp7 <- hc.local(x = list.tp[[1]], positions = list.tp[[2]], distance = 7, plotrestrictions = T)
plot.restrictedgraph(learnt.tp7, positions = list.tp[[2]], distance = 7, node =10, dev = TRUE)
plot.restrictedgraph(learnt.tp7, positions = list.tp[[2]], distance = 7, node =-1) # plots only nodes and arcs , fixed positions

#plot(learnt.tp)

learnt.tp <- hc(x = list.tp[[1]])  # original hc, no restrictions
plot.restrictedgraph(learnt.tp, positions = list.tp[[2]])


learnt.tp10 <- hc.local(x = list.tp[[1]], positions = list.tp[[2]], distance = 10, plotrestrictions = T)
plot.restrictedgraph(learnt.tp10, positions = list.tp[[2]], distance = 10, node =10)

learnt.tp5 <- hc.local(x = list.tp[[1]], positions = list.tp[[2]], distance = 5, plotrestrictions = T)
plot.restrictedgraph(learnt.tp5, positions = list.tp[[2]], distance = 5, node =10)

learnt.tp2 <- hc.local(x = list.tp[[1]], positions = list.tp[[2]], distance = 2, plotrestrictions = T)
plot.restrictedgraph(learnt.tp2, positions = list.tp[[2]], distance = 2, node =10)

scoreS <- c(score(learnt.tp, list.tp[[1]]),score(learnt.tp10, list.tp[[1]]),score(learnt.tp7, list.tp[[1]]), score(learnt.tp5, list.tp[[1]]), score(learnt.tp2, list.tp[[1]]) )


bench <- array(0, 5)
distanceS <- c(10,7,5,2)
bench[1] <-   mean(microbenchmark(hc(x = list.tp[[1]]), times = 25)$time)
for (i in seq(2,5)){
  bench[i] <-   mean(microbenchmark(hc.local(x = list.tp[[1]], positions =list.tp[[2]], distance = distanceS[i-1]), times = 25)$time)
}

plot(scoreS)
plot(bench/(10^9), pch =10)

scoreS
#[1] -33415.78 -33470.44 -33645.73 -33861.98 -35048.10
bench/(10^9)
#[1] 2.9896195 2.5970189 1.6455378 0.8825255 0.1424117 (seg)



###
### hc.local vs hc.local2
###

bench1 <- array(0, 4)
bench2 <- array(0,  4)

distanceS <- c(10,7,5,2)

for (i in seq(1,4)){
  bench1[i] <- mean(microbenchmark(hc.local(x = list.tp[[1]], positions =list.tp[[2]], distance = distanceS[i]), times = 50)$time)
  bench2[i] <- mean(microbenchmark(hc.local2(x = list.tp[[1]], positions =list.tp[[2]], distance = distanceS[i]), times = 50)$time)
}
bench1
#[1] 2593637062 1647512156  856225297  144091632
bench2
#[1] 2581355760 1618492574  824999451  112635071  #ligera mejora de unas centesimas

