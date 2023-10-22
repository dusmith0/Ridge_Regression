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
##++++++## Below here is more complex, but not working. 
  #for(i in 1:nrow(X)){
  #  obj[i] <- log(find.soft(X[i,],beta_init,K = Y[i]))
  #}
  #With mapply
  #mapply(function(q,p){
  #  obj <- log(soft(X,beta_init,K = Y))}, X, Y
  #)
  
  #with apply
  #apply(X, 2, function(v){
  #})
##++++++###Above here
 
  obj <- rep(0,nrow(X))
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
create_w <- function(soft,j){
  n <- nrow(X)
  diag(x = (soft[,j] * (1 - soft[,j])),nrow = n) #For some reason this fails is I use nrow(X) inside of another function here.
}

find.hessian <- function(X, soft, lambda, eta, j){
  I <- diag(x = 1, nrow = ncol(X), ncol = ncol(X)) ##Check the size of I it might be off. 
  hessian <- eta * solve(t(X) %*% create_w(soft,j) %*% (X)+lambda * I) 
  return(hessian)
  }


##To calculate the gradiant-first matrix derivative  ##is is created to loop through elements j
find.gradiant <- function(X, lambda, beta_init, j){
  val <- rep(0,nrow(X))
  for(i in 1:nrow(soft)){ #This can be accomplished with an apply.
    val[i] <- (soft[i,Y[i]] - 1)# Not certain that this is correct or the correct sign.
  }
  val <- matrix(val,ncol=1)
  
  
  gradiant <- crossprod(X,val) + lambda * beta_init[j,]
  return(gradiant)
}


##To calculate the error estimates
Estimate_Prediction <- sqrt(sum((Y - X %*% beta_init) ^ 2))


