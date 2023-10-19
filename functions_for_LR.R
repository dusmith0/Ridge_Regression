#Functions that might be useful 

## Calculating first 

##Apparently this is called a softmax probability
   ##This will store the probability of each input of Xs as a probability against each 
   ##possible K value (Where K values are each column, and Xs are each row.)
soft <- function(X,beta_init,K){
  z <- rep(0,3)
  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
  for(i in 1:nrow(X)){ #I might be able to make this a nested apply instead of a for loop.
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
   + (lambda / 2) * sum(apply(beta_init, c(1,2), function(z) z ^ 2)) #This seemed to work if we want a single number

    #apply(beta_init, c(1,2), function(z) (z ^ 2))
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
gradiant <- function(X, lambda, beta_init, K){
  gradiant <- t(X)%*%(soft[K,] - Y[K]) + lambda * beta_init[k,]
  return(gradiant)
}


##To updateded betas


