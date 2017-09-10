c.table <- function(predicted, real, output.ratios = TRUE){
  real <- matrix(as.numeric(real), ncol = NCOL(real) )
  predicted <-matrix(as.numeric(predicted), ncol = NCOL(predicted) )
  ct1 <- table( predicted - 2*real)
  r1 <- as.numeric(c(ct1["0"], ct1["-2"] ))
  r2 <- as.numeric(c(ct1["1"], ct1["-1"] ))
  CT <- matrix( c(r1,r2 ), ncol = 2, byrow = TRUE)
  colnames(CT) <- c("w0", "w1")
  rownames(CT) <- c("p0", "p1")
  if (!(output.ratios)){
    return(CT)
  }
  else{
    return( list(CT = CT, 
                 FPR = CT[2,1]/(CT[2,1]+CT[1,1]), 
                 FNR = CT[1,2]/(CT[1,2]+CT[2,2]), 
                 MSC = (CT[2,1] + CT[1,2])/sum(CT)) 
          )
  }
}