#Functions that might be useful 

## Calculating first 

##Apparently this is called a softmax probability
soft <- function(X,Beta){
  z <- (X%*%Beta) ##I do not think this bit is correct. But it is producing values.
  soft <- exp(z) / sum(exp(z)) #this naming method will not store the values, just produce them.
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


