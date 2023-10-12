#Functions that might be useful 

##Apparently this is called a softmax probability
soft <- function(z){
  soft <- exp(z) / sum(exp(z))
  return(soft)
}

##To calculate the Objective Function
gradiant <- function(X, k, beta, lambda, soft)

##To calculate the Hessian-second matrix derivative
hessian <- function(X, lambda, eta){
  I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
  W <- diag(x = (soft * (1 - soft)), nrow = nrow(X), ncol = nrow(X))
  hessian <- eta * solve(t(X) %*% W %*% (X)+lamda %*% I) ##I have no idea what Wk is supposed to be????
  return(hessian)
  }


##To calculate the gradiant-first matrix derivative


##To updateded betas
