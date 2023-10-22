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
  
  # Check that the first column of X and Xt are 1s, if not - display appropriate message and stop execution.
  if(sum(X[,1]) != nrow(X)){
    stop(paste("Please include a row of intercept values being 1"))
  }
  # Check for compatibility of dimensions between X and Y
  if(nrow(X) != nrow(Y)){
    stop(paste("Error: Your supplied responce does not match you length of data."))
  }
  # Check for compatibility of dimensions between Xt and Yt
  if(nrow(Xt) != nrow(Yt)){
    stop(paste("Error: Your supplied test Y does not match you length of test X data."))
  }
  # Check for compatibility of dimensions between X and Xt
  
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
  ##Seting some values
  objective <- rep(0,51)
  error_test <- rep(0,51)
  error_train <- rep(0,51)
  
  ##Initial Calculations
  find.soft(X, beta_init,K)
  objective[1] <- find.objective(soft,K,beta_init,lambda)
  
  ##Find training Error
  
  ##Find testing Error
  
  
  ## Newton's method cycle - implement the update EXACTLY numIter iterations
  ##########################################################################
  count <- 0
  beta <- beta_init
  while(count < numIter){
    count <- count + 1
    
    for(j in 1:length(K)){
      beta[j,] <- beta[j,] - find.hessian(X, soft, lambda, eta, j)%*%find.gradiant(X, lambda, beta, j)
    }
    
    find.soft(X, beta,K)
    
    objective[count + 1] <- find.objective(soft,K,beta,lambda)
    
    train_error[count + 1] <- find.error()
    
  }
    
 
  # Within one iteration: perform the update, calculate updated objective function and training/testing errors in %
  
  
  ## Return output
  ##########################################################################
  # beta - p x K matrix of estimated beta values after numIter iterations
  # error_train - (numIter + 1) length vector of training error % at each iteration (+ starting value)
  # error_test - (numIter + 1) length vector of testing error % at each iteration (+ starting value)
  # objective - (numIter + 1) length vector of objective values of the function that we are minimizing at each iteration (+ starting value)
  return(list(beta = beta, error_train = error_train, error_test = error_test, objective =  objective))
}