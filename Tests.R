# This is a script to save your own tests for the function
source("FunctionsLR.R")

##Trying to learn how to use the soft max correctly
X <- matrix(c(1:10),ncol=2)
Beta <- as.matrix(c(2,3))
for(i in 1:nrow(X)){
  X[i,] <- t(X[i,])*2
  
}