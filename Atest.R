
source("functions/downscaling/aux_functions/c.table.R")

obs.dataset <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_53_ECAD_Germany_v1.zip")
local <- loadValueStations(dataset = obs.dataset, var = "precip"  )
localPRED <- loadValueStations(dataset = "data/VALUE_ERA_INTERIM075_53_Germany_v1.zip", var = "precip"  )

local$Data[ local$Data <= 1] <-  0
local$Data[ local$Data > 1 ] <-  1

localPRED$Data[ localPRED$Data <= 1] <-  0
localPRED$Data[ localPRED$Data > 1 ] <-  1

####

c.table(localPRED$Data ,local$Data)
####
####
####

vole <- miMat.VALUE(local,  season = c("annual"))

station.labels <- attr(vole[[1]], "station_names")
scales.list <- list(x = list(labels = station.labels, rot = 90,
                             at = seq(1,ncol(vole[[1]]),1), cex = .5),
                    y = list(labels = station.labels,
                             at = seq(1,ncol(vole[[1]]),1), cex = .5))
lattice::levelplot(vole[[5]], ylab = "", xlab = "",
                   main = "Mutual Information Matrix", scales = scales.list)

