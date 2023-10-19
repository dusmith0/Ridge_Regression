#Functions that might be useful 

## Calculating first 

##Apparently this is called a softmax probability
   ##This will store the probability of each input of Xs as a probability against each 
   ##possible K value (Where K values are each column, and Xs are each row.)
soft <- function(X,beta_init,K=1){
  z <- rep(0,3)
  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
  for(i in 1:nrow(X)){
    z <- apply(beta_init, 1, function(beta_init) (X[i,]%*%(beta_init))) 
    soft[i,] <- exp(z) / sum(exp(z)) 
  }
  return(soft)
}


##To calculate the Objective Function
objective <- function(X, K, Beta, lambda, soft){
  
  for(i in n){
    for(j in K){
      sumlog(soft(j))
    }
  }
  
  
  return(objective)
  
}

##To calculate the Hessian-second matrix derivative
hessian <- function(X, lambda, eta){
  I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
  W <- diag(x = (soft * (1 - soft)),nrow = ncol(X)) ##this is not working
  hessian <- eta * solve(t(X) %*% W %*% (X)+lamda %*% I) 
  return(hessian)
  }


##To calculate the gradiant-first matrix derivative
gradiant <- function(X, lambda, beta, K){
  gradiant <- t(X)%*%(soft(k) - Y[K]) + lambda * beta
  return(gradiant)
}


##To updateded betas


