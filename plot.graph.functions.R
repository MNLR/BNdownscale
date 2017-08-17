plot.graphrestrictions <- function(nodes, positions, distance ) {
  plot.new()

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
    miny <- min(positions[2 ,  ])
    maxy <- max(positions[2 ,  ])
  }

  NodeList <- data.frame(nodes, positions[1, ] , positions[2, ])
  EdgeList <- data.frame(from = numeric(0), to= integer(0))
  a <- graph_from_data_frame(vertices = NodeList, d = EdgeList)

  color=c("red", "blue", "green", "yellow", "brown", "black", "pink", "cyan")
  plot.igraph(a, layout=t(positions), vertex.size=4, vertex.color=color,  rescale=F,  xlim=c(minx, maxx), ylim=c(miny, maxy), asp=FALSE , axes = TRUE)
  mapply(plotellipse, mid = split(positions, rep(1:ncol(positions), each = nrow(positions))), lcol = color , MoreArgs = list( rx = distance, ry = distance, asp = FALSE))
}


plot.restrictedgraph <- function(graph, positions, distance = -1, nodes = 0) {
  # Plots the graph of class bn with nodes in positions and shows the nodes dependance distance as a circle, for a given distance d assumed to be euclidean distance. 
  #  ---- INPUT:
  # graph             An object of class bn whose Directed Acyclic Graph is going to be plotted.
  # positions         Sorted array of the locations of x, can only be 1 or 2 dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x, the data.frame used to learn graph.
  # distance          Maximum distance of dependancy. If no distance is given, no circle will be drawn (so nodes argument will be ignored)
  # nodes             Index of nodes whose dependancy is going to be shown, can be a vector for several nodes. By default, nodes = 0 plots circles for all nodes. -1 will plot no circle, still
  #                   plotting nodes in given positions.
  
  plot.new()
  nodes_ <- names(graph$nodes)
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
    miny <- min(positions[2 ,  ])
    maxy <- max(positions[2 ,  ])
  }
  
  NodeList <- data.frame(nodes_, positions[1, ] , positions[2, ])
  EdgeList <- data.frame(graph$arcs)
  a <- graph_from_data_frame(vertices = NodeList, d = EdgeList)
  
  if (nodes == 0) { cpositions <- positions}
  else {  cpositions <- as.matrix(positions[ ,nodes] ) }
  
  color=c("red", "blue", "green", "yellow", "brown", "black", "pink", "cyan")
  plot.igraph(a, layout=t(positions), vertex.size=4, vertex.color=color,  rescale=F,  xlim=c(minx, maxx), ylim=c(miny, maxy), asp=FALSE , axes = TRUE)
  if (nodes != -1 & distance != -1) {
    trash <- mapply(plotellipse, mid = split(cpositions, rep(1:ncol(cpositions), each = nrow(cpositions))), lcol = color , MoreArgs = list( rx = distance, ry = distance, asp = FALSE))
  }
}




## - Hacer que hc.local devuelva también las posiciones de los nodos y la distancia negra para el plot automatico con igraph
##   (si no fastidia lo de bnlearn?)
##
