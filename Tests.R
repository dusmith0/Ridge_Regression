# This is a script to save your own tests for the function
source("FunctionsLR.R")

##Trying to learn how to use the soft max correctly
X <- matrix(c(1:10),ncol=2)
Beta <- as.matrix(c(2,3))
for(i in 1:nrow(X)){
  X[i,] <- t(X[i,])*2
  
}

##Trying to build a second set of data to work with my functions
a <- rep(1,10)
b <- c(1,2,3,4,5,6,7,8,9,10)
c <- c(2,2,3,3,4,4,5,5,6,6)
X <- matrix(c(a,b,c),ncol=3)

y <- c(1,1,2,2,3,4,3,2,1,3)

##Running individual pieces
#generating K
K <- sort(unique(y))
#generating beta_init
beta_init = NULL
if(is.null(beta_init) == TRUE){
  beta_init <- matrix(0, ncol = ncol(X), nrow = length(K))
}
#trying softmax
soft <- function(X,beta_init){
  z <- (X%*%beta_init) ##I do not think this bit is correct. But it is producing values.
  soft <- exp(z) / sum(exp(z)) #this naming method will not store the values, just produce them.
  return(soft)
}

soft(X,beta_init)

#Testing objective function:
a <- c(1,2,3)
b <- c(1,2,3)
beta_init <- matrix(c(a,b),nrow=3) 





