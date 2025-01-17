# Application of multi-class logistic to letters data

# Load the letter data
#########################
# Training data
letter_train <- read.table("Data/letter-train.txt", header = F, colClasses = "numeric")
Y <- letter_train[, 1]
y <- Y 
X <- as.matrix(letter_train[, -1])

# Testing data
letter_test <- read.table("Data/letter-test.txt", header = F, colClasses = "numeric")
Yt <- letter_test[, 1]
yt <- Yt 
Xt <- as.matrix(letter_test[, -1])

# [ToDo] Make sure to add column for an intercept to X and Xt
V1 <- rep(1,2000)
X <- cbind(V1,X)
Xt <- cbind(V1,Xt)

# Source the LR function
source("FunctionsLR.R")

# [ToDo] Try the algorithm LRMultiClass with lambda = 1 and 50 iterations. Call the resulting object out, i.e. out <- LRMultiClass(...)
out <- LRMultiClass(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL)

# The code below will draw pictures of objective function, as well as train/test error over the iterations
plot(out$objective, type = 'o')  #Final objective was 1779.076
plot(out$error_train, type = 'o') #Final error_train was 21.70
plot(out$error_test, type = 'o') #Final error_test was 26.23889

# Feel free to modify the code above for different lambda/eta/numIter values to see how it affects the convergence as well as train/test errors

# [ToDo] Use microbenchmark to time your code with lambda=1 and 50 iterations. To save time, only apply microbenchmark 5 times.
library("microbenchmark")
microbenchmark(
  LRMultiClass(X, y, Xt, yt, numIter = 50, eta = 0.1, lambda = 1, beta_init = NULL),
  times = 5L
)
# [ToDo] Report the median time of your code from microbenchmark above in the comments below

# Median time:  (in sec) 90.96083 seconds. My code is still extremely slow :/


