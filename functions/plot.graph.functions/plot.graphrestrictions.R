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