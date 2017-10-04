distance.haversine <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  sqrta <- sqrt(a)
  aux <-  sapply(sqrta, FUN =  function(x) min(1,x)  )
  c <- 2 * asin( aux  )
  d = R * c
  return(d) 
}