preprocess.list <- function(...) {
  # preprocess transformeR data given as list
  argS = list(...)
  dataS <- lapply(argS, preprocess.one)
  positions <- do.call(cbind, lapply(dataS, function(x) x[[2]]))
  data <- do.call(cbind.data.frame, c(lapply(dataS, function(x) x[[1]]) , make.row.names = TRUE) )
  return( list(as.data.frame(data) , positions ) )
}