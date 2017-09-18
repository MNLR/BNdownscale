
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

a <- MI.vs.distance(local)
plot(a[1, ], a[ 2, ])

b <- MI.vs.distance(localPRED)
points(b[1, ], b[2, ], col = "blue")
####
####


station.labels <- attr(vole[[1]], "station_names")
scales.list <- list(x = list(labels = station.labels, rot = 90,
                             at = seq(1,ncol(vole[[1]]),1), cex = .5),
                    y = list(labels = station.labels,
                             at = seq(1,ncol(vole[[1]]),1), cex = .5))

dev.new()
lattice::levelplot(vole[[1]], ylab = "", xlab = "",
                   main = "Mutual Information Matrix", scales = scales.list)

