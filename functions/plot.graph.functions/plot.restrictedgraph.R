plot.restrictedgraph <- function(bn, positions, distance = NULL, nodes = -1, node.size = 4,   edge.arrow.size = 0.15 ,dev = FALSE, xlab = "x", ylab = "y") {
  # Plots the graph of class bn with nodes in positions and shows the nodes dependance distance as a circle, for a given distance d assumed to be euclidean distance. 
  #  ---- INPUT:
  # graph             An object of class bn whose Directed Acyclic Graph is going to be plotted.
  # positions         Sorted array of the locations of x, can only be 1 or 2 dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x, the data.frame used to learn graph.
  # distance          Maximum distance of dependancy. If no distance is given, no circle will be drawn (so nodes argument will be ignored)
  # nodes             Index of nodes whose dependancy is going to be shown, can be a vector for several nodes. By default, nodes = 0 plots circles for all nodes. -1 will plot no circle, still
  #                   plotting nodes in given positions.
  if (dev) {  dev.new()  }
  else { plot.new() }
  
  nodes_ <- names(bn$nodes)
  if (NROW(positions) == 1){
    positions <- rbind(positions, 0)
    minx <- min(positions)
    maxx <-  max(positions)
    miny <- -distance
    maxy <- distance
  }
  else {
    minx <- min(positions[1 , ])
    maxx <- max(positions[1 , ])
    miny <- min(positions[2 , ])
    maxy <- max(positions[2 , ])
  }
  
  Nnodes <- length(bn$nodes)
  
  
  NodeList <- data.frame(nodes_, positions[1, ] , positions[2, ])
  EdgeList <- data.frame(bn$arcs)
  a <- graph_from_data_frame(vertices = NodeList, d = EdgeList)
  color <- c("red", "blue", "green", "yellow", "brown", "black", "pink", "cyan")
  color <- array( color, Nnodes)
  
  if (length(nodes) == 1 && nodes == 0){
    nodes <- seq(1, Nnodes)
  }
  if ( (length(nodes) == 1 && nodes != -1) | (length(nodes) != 1) ) { 
    cpositions <- as.matrix(positions[ ,nodes] )
  }
  
  plot.igraph(a, layout=t(positions), vertex.size = node.size, vertex.color=color,  rescale=F,  xlim=c(minx, maxx), ylim=c(miny, maxy), xlab = xlab, ylab = ylab, asp=FALSE , axes = TRUE , edge.arrow.size = edge.arrow.size)
  if ( (length(nodes) == 1 && (nodes != -1 & !(is.null(distance))) ) | ( length(nodes) != 1 & !(is.null(distance)) )  ) {
    trash <- mapply(plotellipse, mid = split(cpositions, rep(1:ncol(cpositions), each = nrow(cpositions))), lcol = color[nodes] , MoreArgs = list( rx = distance, ry = distance, asp = FALSE))
  }
  
}