
## pruebas de learnbn.local
##
##   # comprobar whitelist conflictiva con blacklist

##  

library(bnlearn)

positions <- matrix(c(c(2,2), c(3,4)), ncol = 2)
positions
cosa <- hc.local2(x = cars, positions, 1)
cosa

positions <- matrix(c(c(2,3), c(3,3), c(4,5), c(10,10), c(20,35)), ncol = 5, dimnames = list(NULL, colnames(iris)) )
positions
learning.complement(nodes.centre = 1,nodes = colnames(iris), positions = positions, distance = 4, norm_ = "2")


positions <- matrix(c(c(2,3), c(5,10), c(3,4), c(2,2), c(3,4)), ncol = 5)
positions
cosa1 <- hc.local(x = iris, positions, 5)
cosa1
cosa2 <- hc.local2(x = iris, positions, 0.1)
plot(cosa2)


survey <- read.table("survey.txt", header = TRUE)
colnames(survey) #hay que poner 6 distancias
positions <- matrix(c(c(2,2,10,12,13, 20), c(3,4,5,6,7, 20)), nrow = 2)
positions
learnt.local <- hc.local(survey, positions, 10) # = hc(survey)
learnt.local
