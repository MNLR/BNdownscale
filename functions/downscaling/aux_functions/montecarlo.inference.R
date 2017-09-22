######
######
######
global <- testPRED

downscale.BN <- DBN

BN <- downscale.BN$BN
BN.fit <- downscale.BN$BN.fit
clusterS <- downscale.BN$clusterS
mode_ <- downscale.BN$mode
mode <- as.numeric(mode_[1])
mode2 <- mode_[2]
Nglobal <- downscale.BN$Nglobals
predictors <- names(BN$nodes)[1:Nglobal]
predictands <- names(BN$nodes)[- (1:Nglobal) ]
clustering.attributes <- downscale.BN$clustering.attributes


if (is.null(clusterS)){
  if (mode2 == "1"){  clustered <- as.matrix(preprocess(global)[[1]]) }  else { clustered  <- categorize.bn( global, mode, NULL , clustering.attributes)[[1]] }
}
else{
  p.global <- preprocess.forKmeans(global, mode, scale.args = clustering.attributes )
}

if (mode == 3) {
  clustered <- as.factor( predict(clusterS, newdata = p.global) )
  PT <- lapply(clustered, FUN = predict.DBN , predictors = predictors, junction = junction, predictands = predictands )
}
else if (mode == 1 | mode == 2){
  if (!(is.null(clusterS))){
    clustered <- mapply(predict , object = clusterS, newdata = p.global ,  SIMPLIFY = TRUE  ) # matrix of data where each column is a node with its "climate value" per observation
  }  
  clustered <- matrix(as.factor(clustered), ncol = NCOL(clustered))  # reconverted to categorical
  PT <- apply(clustered, MARGIN = 1 , FUN = predict.DBN , predictors = predictors, 
              junction = junction , predictands = predictands )
}

######
######
######



conditions <- paste("(", predictors, " == '",
                    sapply(clustered[1,], as.character), "')", 
                    sep = "", collapse = " & ")

cpquery(fitted, eval(parse(text = str2)), eval(parse(text = str)))

table(cpdist(BN.fit, "D.2760", evidence =  eval(parse( text = conditions)) , debug = TRUE))

conditions2 <-  "(G.2006 == '1') & (G.2760 == '1')"

tab <- table(cpdist(BN.fit, "D.2760", evidence = eval(parse(text = conditions ))  , debug = TRUE, method = "ls")) # este va
tab <- table(cpdist(BN.fit, "D.2760", evidence =  (G.2006 == '1') & (G.2760 == '1')  , debug = TRUE)) # este va
tab[1]/sum(tab)
tab[2]/sum(tab)

##### ejemplo ayuda bnlearn

data(learning.test)
fitted = bn.fit(hc(learning.test), learning.test)
# the result should be around 0.025.
cpquery(fitted, (B == "b"), (A == "a"))
# programmatically build a conditional probability query...
var = names(learning.test)
obs = 2
str = paste("(", names(learning.test)[-3], " == '",
            sapply(learning.test[obs, -3], as.character), "')",
            sep = "", collapse = " & ")
str
str2 = paste("(", names(learning.test)[3], " == '",
             as.character(learning.test[obs, 3]), "')", sep = "")
str2

