source("functions/plot.graph.functions/plot.restrictedgraph.R")
plot.DBN <- function(DBN, nodes = -1, node.size = 4, edge.arrow.size = 0.15, dev = FALSE ){
  plot.restrictedgraph( bn = DBN$BN , positions = DBN$positions, distance = DBN$bnlearning.args.list$distance, 
                        nodes = nodes, node.size = node.size, edge.arrow.size = edge.arrow.size , dev = dev)
}