source("functions/local.bnlearning/aux_functions/build.distanceBlacklist.R")

mmhc.local <- function(x, positions, distance, norm = "2", exceptions = NULL, plotrestrictions = FALSE, whitelist = NULL, blacklist = NULL, restrict = "gs", maximize = "hc",
                         test = NULL, score = NULL, alpha = 0.05, B = NULL, ...,
                         maximize.args = list(), optimized = TRUE, strict = FALSE, debug = FALSE){
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
  return( mmhc(x = x, whitelist = whitelist, blacklist = blacklist, restrict = restrict , maximize = maximize,
                 test = test, score = score, alpha = alpha, B = B, ...,
                 maximize.args = maximize.args, optimized = optimized, strict = strict, debug = debug) )
}