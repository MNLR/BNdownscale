source("functions/local.bnlearning/aux_functions/build.distanceBlacklist.R")

inter.iamb.local <- function(x, positions, distance, norm = "2", exceptions = NULL, plotrestrictions = FALSE, 
                       cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
                       alpha = 0.05, B = NULL, debug = FALSE, optimized = TRUE, strict = FALSE,
                       undirected = FALSE ){
  #  ---- INPUT:
  # x                 a data frame containing the variables of the model
  # positions         sorted array of the locations of x, can be N dimensional, must contain the same number of columns as the number of variables in x, and number of rows is the dimension.
  #                   Won't check column names, positions must be sorted the same way as the columns (Variables) of x
  # distance          maximum distance at which nodes will be checked for a directed arc
  # norm              norm for distance. format: same as norm(), check ?norm: "2" euclidean, ... . May be a function in the form   (...)
  # plotrestrictions  A plot with node restrictions will be displayed. It only makes sense if the norm is set to "2". It will be ignored if NROW(positions) > 2 
  #  
  #  ---- OUTPUT:
  #             object of class bn from bnlearn library.
  blacklist <- build.distanceBlacklist( colnames(x), positions, distance, exceptions = exceptions, blacklist = blacklist,
                                        norm = norm, plotrestrictions = plotrestrictions, debug = debug)
  
  return( inter.iamb(x = x, cluster = cluster, whitelist = whitelist, blacklist = blacklist, test = test,
               alpha = alpha, B = B, debug = debug, optimized = optimized, strict = strict,
               undirected = undirected) )
}