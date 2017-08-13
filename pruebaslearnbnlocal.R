
## pruebas de learnbn.local
##
##   # comprobar whitelist conflictiva con blacklist

##  

positions <- matrix(c(c(2,2), c(3,4)), nrow = 2)
positions
cosa <- learnbn.local(x = cars, positions, 2)
cosa

positions <- matrix(c(c(2,2,3,5,10), c(3,4,2,2,10), c(4,4,5,7,10)), nrow = 5)
out <- learning.complement(nodes.centre = 1,nodes = colnames(iris), positions = positions, distance = 4, norm_ = "2")
cosa <- learnbn.local(x = iris, positions, 4)
cosa
cosa[[1]][1]
cosa[[1]][2]



a  matrix(colnames(iris), nrow = 1)
thing <- matrix(colnames(iris), nrow = 1)
a <- rbind(a, thing)

x <- iris
a <- list()
names <- colnames(x)
for (i in 1:(NCOL(x)-1)) {
  
  a[[i]] <- names
  names <- names[2:length(names)]
}


survey <- read.table("survey.txt", header = TRUE)
colnames(survey) #hay que poner 6 distancias
positions <- matrix(c(c(2,2,10,12,13, 20), c(3,4,5,6,7, 20)), ncol = 2)
positions
learnt.local <- learnbn.local(x = survey, positions, 1000) # = hc(survey)
plot(learnt.local)













###############
###############
###############

a <- list()
names <- colnames(x)
for (i in 1:(NCOL(x)-1)) {
  a[[i]] <- names
  names <- names[2:length(names)]
}

for.the.blacklist <- Mapply(FUN =  learning.complement , nodes.centreS[1:length(nodes.centreS)-1] ,  nodes = nodes, positions=positions, distance=distance, norm=norm)
