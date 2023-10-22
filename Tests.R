# This is a script to save your own tests for the function
source("FunctionsLR.R")
library("microbenchmark")

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

Y <- c(1,1,2,2,3,4,3,2,1,3)

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

#Testing objective function:#Not a good test. 
a <- c(1,2,3)
b <- c(1,2,3)
beta_init <- matrix(c(a,b),nrow=3) 


#Again trying to test for the objective function:
#++++++# Run the block below
a <- rep(1,10)
b <- c(1,2,3,4,5,6,7,8,9,10)
c <- c(2,2,3,3,4,4,5,5,6,6)
X <- matrix(c(a,b,c),ncol=3)

Y <- c(1,1,2,2,3,4,3,2,1,3)

beta_init <- NULL

K <- sort(unique(Y))

if(is.null(beta_init) == TRUE){
  beta_init <- matrix(0, ncol = ncol(X), nrow = length(K))
}
#+++++++#



###Checking on some speeds in microbenchmark
microbenchmark(  ##Retuned 182 nano seconds
  find.hessian <- function(X, soft, lambda, eta, j){
    I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
    hessian <- eta * solve(t(X) %*% create_w(soft,j) %*% (X)+lambda * I) 
    return(hessian)
  }
)

microbenchmark(
  find.hessian <- function(X, soft, lambda, eta, j){ ##This does not seem to be any faster.
    I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
    hessian <- eta * solve(crossprod((X),create_w(soft,j))%*%(X)+lambda * I) 
    return(hessian)
  }
)


##Another Test
microbenchmark( #occasionally is slower then below. 
find.gradiant <- function(X, lambda, beta_init, j){
  val <- rep(0,nrow(X))
  for(i in 1:nrow(soft)){ 
    val[i] <- (soft[i,Y[i]] - 1)
  }
  val <- matrix(val,ncol=1)
  
  
  gradiant <- (t(X)%*%val) + lambda * beta_init[j,]
  return(gradiant)
}
)


microbenchmark( #Seems to be around 200 nanoseconds
  find.gradiant <- function(X, lambda, beta_init, j){
    val <- rep(0,nrow(X))
    for(i in 1:nrow(soft)){ 
      val[i] <- (soft[i,Y[i]] - 1)
    }
    val <- matrix(val,ncol=1)
    
    
    gradiant <- crossprod(X,val) + lambda * beta_init[j,]
    return(gradiant)
  }
)

##Trying to speed up my Softmax
find.soft <- function(X,beta_init,K){
  z <- rep(0,ncol(X))
  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
  for(i in 1:nrow(X)){ #I might be able to make this a nested apply instead of a for loop.
    z <- apply(beta_init, 1, function(beta_init) (X[i,]%*%(beta_init))) 
    soft[i,] <- exp(z) / sum(exp(z)) 
  }
  return(soft)
}


find.soft2 <- function(X,beta_init,K){
  z <- rep(0,ncol(X))
  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
  apply(X,1,function(X){
  z <- apply(beta_init, 1, function(beta_init) (X%*%(beta_init))) 
    soft <- exp(z) / sum(exp(z)) 
  })
  return(soft)
}

find.soft3 <- function(X, beta_init) {
  z <- X %*% t(beta_init)  # Matrix multiplication for z values
  soft <- exp(z)
  soft <- soft / rowSums(soft)
  return(soft)
}


microbenchmark(
  find.soft(X,beta_init,K)
)

microbenchmark( #This was surprisingly slightly slower.
  find.soft2(X,beta_init,K)
)

microbenchmark(
  find.soft3(X,beta_init)
)

#Another test for speed on the Objective function

soft <- find.soft(X,beta_init,K)

microbenchmark(
  find.objective(soft,K,beta_init,lambda)
)


#Another test to see if the logical instead of sapply is better
j <- 1 

find.gradiant <- function(X, lambda, beta_init, j){
  val <- soft[,j] - sapply(Y,function(Y) ifelse(Y == K[j],1,0))
  #val <- soft[,j] - (Y == K[j]) #I forgot that we can use logical as 0,1 too.
  gradiant <- crossprod(X,val) + lambda * beta_init[j,]
  return(gradiant)
}

find.gradiantl <- function(X, lambda, beta_init, j){
  #val <- soft[,j] - sapply(Y,function(Y) ifelse(Y == K[j],1,0))
  val <- soft[,j] - 1*(Y == K[j]) #I forgot that we can use logical as 0,1 too.
  gradiant <- crossprod(X,val) + lambda * beta_init[j,]
  return(gradiant)
}

microbenchmark( #4 milliseconds
  find.gradiant(X,lambda,beta_init,j)
)

microbenchmark( #97 microseconds
  find.gradiantl(X,lambda,beta_init,j)
)

