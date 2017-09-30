# Example of a two step bayesian network using gs as first algorithm and hc as second.
DBN <- build.downscalingBN(local, global, categorization.type = "varsClustering",
                           forbid.global.arcs = TRUE,
                           forbid.local.arcs = FALSE,
                           bnlearning.algorithm = "gs", 
                           ncategories = 5,
                           clustering.args.list = list(k = 12, family = kccaFamily("kmeans") ), 
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           bnlearning.args.list = list(test="mc-mi"),
                           param.learning.method = "bayes",
                           two.step = TRUE,
                           return.first = TRUE,
                           bnlearning.algorithm2 = "hc.local",
                           bnlearning.args.list2 = list(distance = 2)
                           )
plot.DBN( DBN$first, dev=TRUE, nodes = c(5), edge.arrow.size = 0.25, node.size = 0)
plot.DBN( DBN$last, dev=TRUE, nodes = c(5), edge.arrow.size = 0.25, node.size = 0)

prediction <- downscale.BN(DBN$last , global , parallelize = TRUE ,  n.cores = 7, prediction.type = "event")
c.table(prediction, real$Data)
c.table.rates(c.table(prediction, real$Data), "all")
auc.DBN(downscaled = downscaled, realData = real$Data, plot.curves = TRUE, points = 100)


DBN <- build.downscalingBN(local, global, categorization.type = "nodeEven",
                           forbid.global.arcs = TRUE,
                           forbid.local.arcs = TRUE,
                           bnlearning.algorithm = "gs", 
                           ncategories = 5,
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           bnlearning.args.list = list(test="mc-mi"),
                           param.learning.method = "bayes",
                           two.step = TRUE,
                           return.first = TRUE,
                           bnlearning.algorithm2 = "hc.local",
                           bnlearning.args.list2 = list(distance = 3)
                          )

# Example of a two step bayesian network using already categorized global and hc.local
DBN <- build.downscalingBN(local, REA, categorization.type = "no",
                           forbid.global.arcs = TRUE,
                           forbid.local.arcs = TRUE,
                           bnlearning.algorithm = "hc.local", 
                           bnlearning.args.list = list(distance=2),
                           parallelize = TRUE, n.cores = 7,
                           output.marginals = TRUE, 
                           param.learning.method = "bayes",
                           two.step = FALSE
                           )