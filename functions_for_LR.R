#Functions that might be useful 

## Calculating first 

##Apparently this is called a softmax probability
   ##This will store the probability of each input of Xs as a probability against each 
   ##possible K value (Where K values are each column, and Xs are each row.)
find.soft <- function(X,beta_init,K){
  z <- rep(0,ncol(X))
  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
  for(i in 1:nrow(X)){ #I might be able to make this a nested apply instead of a for loop.
    z <- apply(beta_init, 1, function(beta_init) (X[i,]%*%(beta_init))) 
    soft[i,] <- exp(z) / sum(exp(z)) 
  }
  return(soft)
}


##To calculate the Objective Function
find.objective <- function(soft, K, beta_init, lambda){
  #to find the first bit... with for loop
  obj <- rep(0,nrow(X))
##++++++## Below here is more complex, but not working. 
  for(i in 1:nrow(X)){
    obj[i] <- log(soft(X[i,],beta_init,K = Y[i]))
  }
  #With mapply
  mapply(function(q,p){
    obj <- log(soft(X,beta_init,K = Y))}, X, Y
  )
  
  #with apply
  apply(X, 2, function(v){
  })
##++++++###Above here
 
  ##This simplistic case seems to work the best... It requires that soft is already found.
  for(i in 1:nrow(soft)){
    obj[i] <- log(soft[i,Y[i]])
  }
  total <- sum(obj)

  #to apply the penalty to the objective
  penalty <-  (lambda / 2) * sum(apply(beta_init, c(1,2), function(z) z ^ 2)) #This seemed to work if we want a single number

    #apply(beta_init, c(1,2), function(z) (z ^ 2))
  objective <- (penalty - total)
  
  return(objective)
}

##To calculate the Hessian-second matrix derivative
find.hessian <- function(X, lambda, eta){
  I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
  W <- diag(x = (soft * (1 - soft)),nrow = ncol(X)) ##this is not working
  hessian <- eta * solve(t(X) %*% W %*% (X)+lamda %*% I) 
  return(hessian)
  }


##To calculate the gradiant-first matrix derivative
find.gradiant <- function(X, lambda, beta_init, K){
  gradiant <- t(X)%*%(soft[K,] - Y[K]) + lambda * beta_init[k,]
  return(gradiant)
}


##To updateded betas


