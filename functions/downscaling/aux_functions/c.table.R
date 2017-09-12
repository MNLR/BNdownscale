c.table <- function(predicted, real){
  real <- matrix(as.numeric(real), ncol = NCOL(real) )
  predicted <-matrix(as.numeric(predicted), ncol = NCOL(predicted) )
  ct1 <- table( predicted - 2*real)
  r1 <- as.numeric(c(ct1["0"], ct1["-2"] ))
  r2 <- as.numeric(c(ct1["1"], ct1["-1"] ))
  CT <- matrix( c(r1,r2 ), ncol = 2, byrow = TRUE)
  colnames(CT) <- c("w0", "w1")
  rownames(CT) <- c("p0", "p1")
  CT[is.na(CT)] <- 0
  
  return(CT)
}

c.table.rates <- function(c.table, value ){
  switch(value,
         TPR = { return( c.table[2,2]/(c.table[2,2]+c.table[1,2]) ) },
         FPR = { return( c.table[2,1]/(c.table[2,1]+c.table[1,1]) ) },
         TNR = { return( c.table[1,1]/(c.table[1,1]+c.table[2,1]) ) },
         FNR = { return( c.table[1,2]/(c.table[1,2]+c.table[2,2]) ) }, 
         MSC = { return( (c.table[2,1] + c.table[1,2])/sum(c.table) ) },
         all = { return( list(TPR = c.table[2,2]/(c.table[2,2]+c.table[1,2]),
                              FPR = c.table[2,1]/(c.table[2,1]+c.table[1,1]),
                              TNR = c.table[1,1]/(c.table[1,1]+c.table[2,1]),
                              FNR = c.table[1,2]/(c.table[1,2]+c.table[2,2]), 
                              MSC = (c.table[2,1] + c.table[1,2])/sum(c.table) )
                        )
                }
         )
}