#Functions that might be useful 

## Calculating first 

##Apparently this is called a softmax probability
   ##This will store the probability of each input of Xs as a probability against each 
   ##possible K value (Where K values are each column, and Xs are each row.)
#find.soft <- function(X,beta_init,K){
#  z <- rep(0,ncol(X))
#  soft <- matrix(rep(0,nrow(X)*length(K)),nrow=nrow(X))
#  for(i in 1:nrow(X)){ #I might be able to make this a nested apply instead of a for loop.
#    z <- apply(beta_init, 1, function(beta_init) (X[i,]%*%(beta_init))) 
#    soft[i,] <- exp(z) / sum(exp(z)) 
#  }
#  return(soft)
#}

#apparently this one is much faster, and actually works. I still confused as to why though.
find.soft <- function(X, beta_init) {
  z <- X %*% t(beta_init)  
  soft <- exp(z)
  soft <- soft / rowSums(soft)
  return(soft)
}

##To calculate the Objective Function
find.objective <- function(soft, K, beta_init, lambda){
  obj <- rep(0,nrow(X))
  ##This simplistic case seems to work the best... It requires that soft is already found.
  for(i in 1:nrow(soft)){
    obj[i] <- log(soft[i,Y[i]])
  }
  total <- sum(obj)

  #to apply the penalty to the objective
  penalty <-  (lambda / 2) * sum(beta_init^2) #This definelty does what the below did but much easier....
  #sum(apply(beta, c(1,2), function(z) z ^ 2)) #This seemed to work if we want a single number
  
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
  hessian <- eta * solve(t(X) %*% create_w(soft,j) %*% (X) + lambda * I) 
  return(hessian)
  }


##To calculate the gradiant-first matrix derivative  ##is is created to loop through elements j
find.gradiant <- function(X, lambda, beta, j){
  #val <- soft[,j] - sapply(Y,function(Y) ifelse(Y == K[j],1,0))
  val <- soft[,j] - (Y == K[j]) #I forgot that we can use logical as 0,1 too.
  gradiant <- crossprod(X,val) + lambda * beta[j,]
  return(gradiant)
}


##To calculate the error estimates
Estimate_Prediction <- sqrt(sum((Y - X %*% beta_init) ^ 2))


