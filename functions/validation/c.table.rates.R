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