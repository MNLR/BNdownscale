plot.aucS <- FALSE
plot.MI <- TRUE
mi.threshold <- 0.35
plot_.DBN <- FALSE
reference <- TRUE
only.loes.third <- FALSE
print.results <- TRUE
BNB.args.list <- list(categorization.type = "nodeSimple",
                      forbid.global.arcs = TRUE,
                      forbid.local.arcs = FALSE,
                      bnlearning.algorithm = "hc.local",
                      bnlearning.args.list = list(distance = 2),
                      ncategories = 9,
                      parallelize = TRUE, n.cores = 7)

source("VALUE.EXPERIMENT.R")
VALUE.EXPERIMENT(BNB.args.list, print.results = print.results, plot.aucS = plot.aucS, plot.MI = plot.MI,
                 mi.threshold = mi.threshold, plot_.DBN = plot_.DBN,
                 only.loes.third = only.loes.third, reference = reference)