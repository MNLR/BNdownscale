source("functions/downscaling/aux_functions/categorize.bn.R")
source("functions/downscaling/aux_functions/add.toBlacklist.R")
source("functions/downscaling/marginals.R")
source("functions/local.bnlearning/hc.local.R")
source("functions/local.bnlearning/tabu.local.R")
source("functions/local.bnlearning/gs.local.R")
source("functions/local.bnlearning/iamb.local.R")
source("functions/local.bnlearning/fast.iamb.local.R")
source("functions/local.bnlearning/inter.iamb.local.R")
source("functions/local.bnlearning/mmpc.local.R")
source("functions/local.bnlearning/si.hilton.pc.local.R")

build.downscalingBN <- function(local, global, categorization.type = "nodeSimple", 
                                forbid.global.arcs = TRUE, forbid.local.arcs = FALSE,
                                ncategories = 3,
                                clustering.args.list = list(),
                                bnlearning.algorithm = "hc",  
                                bnlearning.args.list = list(),
                                param.learning.method = "bayes",
                                output.marginals = TRUE,
                                parallelize = FALSE, n.cores= NULL, cluster.type = "PSOCK",
                                two.step = FALSE,
                                return.first = FALSE,
                                bnlearning.algorithm2 = NULL,
                                bnlearning.args.list2 = NULL
                                ) {
  # global   predictors, expects: a list of predictor datasets, a Multigrid from makeMultiGrid() or a single dataset
  #              It  is asumed that the data is consistent if a list is provided, and only the positions of first element will be used.
  #              Can be categorical or continuous. See mode
  #              2 coordinate postions are expected. 
  #              NaNs are not expected when categorization, using quantiles, is to be done, modes *0 and *2
  # local    predictands. Expects categorical data. 
  #              NaNs will be processed
  # bnlearning.algorithm    Supports all the functions from bnlearn and their .local counterparts. Check their corresponding parameters.
  # bnlearning.args.list    List of arguments passed to bnlearning.algorithm, in particular distance argument if local learning is used.
  # categorization.type     "no"              Use if global data is already categorical.
  #                         "nodeClustering"  Clustering is performed for each node. 
  #                         "varsClustering"  Clustering is performed for each variable and node. 
  #                         "nodeSimple"      Simple categorization by value, per node. 
  #                         "varsSimple"      Simple categorization by value, per variable and node.
  #                         "nodeEven"        Categorization by quantile, per node.  
  #                         "varsEven"        Categorization by quantile, per variable and node.
  #                         "atmosphere"      Clustering is performed for all nodes at once condensing the atmosphere status. 
  # param.learning.method   Either "bayes" or "mle", passed to learn the parameters of the built network structure.
  # 
  # ncategories             Use for categorization.type = "nodeSimple", "varsSimple", "nodeEven", "varsEven", number of categories for each node/variable.
  #                           Caution: For "nodeSimple" and "nodeEven" they will have ncategories^(Nvars)
  # clustering.args.list    list. Use for categorization.type = "atmosphere", "nodeClustering" and "varsClustering". k is the number of node states
  #                           Check flexclust::kcca() function, expects a list of arguments, e.g. list(k = 12, family = kccaFamily("kmeans"))
  #                           
  # output.marginals        Compute and output Marginal Probability distribution Tables 
  # two.step                Learn first local bayesian network, then inject global nodes 
  # return.first            Will return a list with the two bayesian networks.
  # bnlearning.algorithm2   Same as bnlearning.algorithm for the global injection process, ignored if two.step is set to FALSE
  # bnlearning.args.list2   Same as bnlearning.args.list for the global injection process, ignored if two.step is set to FALSE

  if (!(is.character(bnlearning.algorithm))) { stop("Input algorithm name as character") }
  
  if (two.step){ # First step has no global
    data <- preprocess(local, rm.na = TRUE, rm.na.mode = "observations")
    Dpositions <- data[[2]]
  }
  else{
    print("Categorizing data...")
    # CATEGORIZATION 
    if (categorization.type != "no"){ 
      global.prc <- categorize.bn(global, type = categorization.type,
                                  ncategories = ncategories,
                                  clustering.args.list = clustering.args.list, 
                                  parallelize = parallelize, cluster.type = cluster.type, n.cores = n.cores,
                                  training.phase = TRUE)
      global <- list(Data = global.prc$Data, xyCoords = global.prc$xyCoords)
      clusterS <- global.prc$clusterS
      categorization.attributes <- global.prc$categorization.attributes
      Nglobals <- global.prc$Nglobals
    }

    data <- preprocess(local, global, rm.na = TRUE, rm.na.mode = "observations")
  
    if (categorization.type == "no") {
      Nglobals <- length(grep("G", colnames(data[[1]])))
      clusterS <- NULL
      categorization.attributes <- NULL
    }
  
    if (forbid.global.arcs & categorization.type != "atmosphere"){
      globalNodeNames <- colnames(data[[1]][ , 1:Nglobals ])
      bnlearning.args.list <- add.toBlacklist(globalNodeNames, bnlearning.args.list)
    }
    if (forbid.local.arcs){
      localNodeNames <- colnames(data[[1]][ , (Nglobals+1):NCOL(data[[1]]) ])
      bnlearning.args.list <- add.toBlacklist(localNodeNames, bnlearning.args.list)
    }
    print("Done.")
  }
  # For local learning positions need to be inputed
  if ( substr(bnlearning.algorithm, nchar(bnlearning.algorithm)-5+1, nchar(bnlearning.algorithm)) == "local" ){
    bnlearning.args.list[["positions"]] <- data[[2]]
    if (categorization.type == "atmosphere"){
      if ( !( is.null(bnlearning.args.list$exceptions) ) ){
        bnlearning.args.list$exceptions <- c(bnlearning.args.list$exceptions,  1 )
      }
      else {bnlearning.args.list[["exceptions"]] <- 1 }
    }
  }

  data[[1]][] <- lapply( data[[1]], as.factor) 
  bnlearning.args.list[["x"]] <- data[[1]]
  
  print("Building Bayesian Network...")
  alg <- strsplit(bnlearning.algorithm, split = ".", fixed = TRUE)[[1]][1]
  if ( (alg != "hc")  & (alg != "tabu") ) { 
    cl <- NULL
    if ( parallelize ) { # constraint-based algorithms allow parallelization
      if ( is.null(n.cores) ){
        n.cores <- floor(detectCores()-1)
      }
      # Initiate cluster
      cl <- makeCluster(n.cores, type = cluster.type )
      bnlearning.args.list[["cluster"]] <- cl
    }
    bn <- cextend( do.call(bnlearning.algorithm, bnlearning.args.list) )
    if (!(is.null(cl))) {stopCluster(cl)}
  }
  else { bn <-  do.call(bnlearning.algorithm, bnlearning.args.list) }
  if (!two.step){ bn.fit <- bn.fit(bn, data = data[[1]], method = param.learning.method) }
  print("Done.")
  
  if ( two.step ){
    print("Injecting Globals into Bayesian Network...")
    whitelist <- apply( bn$arcs, MARGIN = 2,function(x) paste0("D.", x) ) 

    if (is.null(bnlearning.algorithm2) ){ bnlearning.algorithm2 <- bnlearning.algorithm} 
    if (is.null( bnlearning.args.list2 ) ){ bnlearning.args.list2 <- bnlearning.args.list}
    
    if ( is.null(bnlearning.args.list2$whitelist) ){ bnlearning.args.list2[["whitelist"]] <- whitelist }
    else{ rbind(whitelist, bnlearning.args.list2$whitelist) }

    DBN <-  build.downscalingBN(local, global, categorization.type = categorization.type, 
                                forbid.global.arcs = forbid.global.arcs,  
                                forbid.local.arcs = forbid.local.arcs,
                                ncategories = ncategories,
                                clustering.args.list = clustering.args.list,                                
                                bnlearning.algorithm = bnlearning.algorithm2,
                                bnlearning.args.list = bnlearning.args.list2,
                                parallelize = parallelize, n.cores= n.cores, cluster.type = cluster.type,
                                output.marginals = output.marginals,
                                param.learning.method = param.learning.method,
                                two.step = FALSE)
    if (return.first){ 
      return( list(first = list(BN = bn, positions = Dpositions,  bnlearning.args.list = bnlearning.args.list), 
                   last = DBN) )
    } 
    else { return(DBN) }
  }  
  else {
    if (output.marginals){
      print("Computing Marginal Distributions...")
      marginals_ <- marginals( list(BN = bn, BN.fit = bn.fit, Nglobals = Nglobals) )
      print("Done.")
    }
    else {marginals_ <- NULL}
  
    return( list(BN = bn, training.data = data[[1]], positions = data[[2]], BN.fit = bn.fit, 
                 categorization.type = categorization.type,
                 clusterS = clusterS, 
                 categorization.attributes = categorization.attributes, 
                 Nglobals = Nglobals,
                 marginals = marginals_,
                 bnlearning.algorithm = bnlearning.algorithm,
                 clustering.args.list = clustering.args.list,
                 bnlearning.args.list = bnlearning.args.list,
                 param.learning.method = param.learning.method)    
            )
  }
}


