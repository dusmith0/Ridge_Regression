# Function that implements multi-class logistic regression.
#############################################################
# Description of supplied parameters:
# X - n x p training data, 1st column should be 1s to account for intercept
# y - a vector of size n of class labels, from 0 to K-1
# Xt - ntest x p testing data, 1st column should be 1s to account for intercept
# yt - a vector of size ntest of test class labels, from 0 to K-1
# numIter - number of FIXED iterations of the algorithm, default value is 50
# eta - learning rate, default value is 0.1
# lambda - ridge parameter, default value is 1
# beta_init - (optional) initial starting values of beta for the algorithm, should be p x K matrix 

## Return output
##########################################################################
# beta - p x K matrix of estimated beta values after numIter iterations
# error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
# error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
# objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)

LRMultiClass <- function(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL){
  source("functions_for_LR.R")
  ## Check the supplied parameters as described. You can assume that X, Xt are matrices; y, yt are vectors; 
  ## and numIter, eta, lambda are scalars. You can assume that beta_init is either NULL (default) or a matrix.
  ###################################
  # Building values of K
  K <- sort(unique(y))
  
  # Adjusting for interval counting at 0 to 1
  if(0 %in% K){
    K <- K + 1
    y <- y + 1
  }
  
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(sum(X[,1]) != nrow(X)){
    stop(paste("Please include a row of intercept values being 1"))
  }
  # Check for compatibility of dimensions between X and Y
  if(nrow(X) != length(y)){
    stop(paste("Error: Your supplied responce does not match you length of data."))
  }
  # Check for compatibility of dimensions between Xt and Yt
  if(nrow(Xt) != length(yt)){
    stop(paste("Error: Your supplied test Y does not match you length of test X data."))
  }
  # Check for compatibility of dimensions between X and Xt
  if(ncol(X) != ncol(Xt)){
    stop(paste("Error: Your testing and training data have different beta values"))
  }
  # Check eta is positive
  if(eta < 0){
    stop(paste("Error: Please input a positive learning rate"))
  }
  # Check lambda is non-negative
  if(lambda < 0){
    stop(paste("Error: Please input a positive penalty value (labmda value)"))
  }
  # Check whether beta_init is NULL. If NULL, initialize beta with p x K matrix of zeroes.
  if(is.null(beta_init) == TRUE){
    beta_init <- matrix(0, ncol = ncol(X), nrow = length(K))
  }
  # If not NULL, check for compatibility of dimensions with what has been already supplied.
  if(nrow(beta_init) != length(K) | ncol(beta_init) != ncol(X)){
    stop(paste("Error: You initial beta choice is not a compatable length with X or K."))
  }
  ## Calculate corresponding pk, objective value f(beta_init), training error and testing error given the starting point beta_init
  ##########################################################################
  ## Seting some values
  objective <- rep(0,numIter + 2)
  error_test <- rep(0,numIter + 2)
  error_train <- rep(0,numIter + 2)
  
  ## Initial Calculations
  soft <- find.soft(X,beta_init)
  objective[1] <- find.objective(soft,K,beta_init,lambda)
  
  ## Find training Error
  error_train <- find.error(soft,y)
  ## Find testing Error
  soft_test <- find.soft(Xt,beta_init)
  error_test <- find.error(soft_test,yt + 1)
  
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  beta <- beta_init
  for(i in 1:numIter){
    for(j in 1:length(K)){
      beta[j,] <- beta[j,] - find.hessian(X, soft, lambda, eta, j) %*% find.gradiant(X, lambda, beta, j)
    }
    soft <- find.soft(X, beta)
    objective[i + 1] <- find.objective(soft,K,beta,lambda)
  }
    
 
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  for(j in 1:length(K)){
    beta[j,] <- beta[j,] - find.hessian(X, soft, lambda, eta, j)%*%find.gradiant(X, lambda, beta, j)
  }
  
  soft <- find.soft(X, beta)
  objective[numIter + 2] <- find.objective(soft,K,beta,lambda)
  error_train[numIter + 2] <- find.error(soft,y)
  
  soft_test <- find.soft(Xt,beta)
  error_test[numIter + 2] <- find.error(soft_test,yt + 1) 
  #note because the iterations of y is y + 1, yt also needs + 1 to match the class iteration of this code.
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}
