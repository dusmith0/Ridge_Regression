**Attention: Because math rendering of .Rmd is not ideal, please see
the enclosed pdf for correct rendering of all equations.**

## Multi-class logistic regression

Logistic regression is one of the most popular classifiers. Consider the
training data consisting of $\(n\)$ samples $\((x_i, y_i)\)$,
$\(x_i\in \mathbb{R}^p\)$, $\(y_i\in \{0, \dots, K-1\}\) (\(K\)$ classes,
the coding starts from 0). For each class \(k\in\{0, \dots, K-1\}\), we
consider class probabilities for sample \(i\) conditioning on the
corresponding vector of covariates \(x_i\): \[
P(y_i = k|x_i) = p_k(x_i), \quad \sum_{k=0}^{K-1}p_k(x_i) = 1.
\] We assume the following model holds for class probabilities \[
p_k(x_i) = \frac{e^{x_i^{\top}\beta_k}}{\sum_{l=0}^{K-1}e^{x_i^{\top}\beta_l}}.
\] Unlike binary case where there is only one \(\beta\), we now have
\(K\) vectors \(\beta_k \in \mathbb{R}^p\), \(k\in \{0,\dots, K-1\}\),
one for each class. Because of the constraint that class probabilities
sum to one, the above model is over-parametrized (in binary logistic, we
had only one \(\beta\) rather than two, as we can always express one
probability as 1 minus all the others). Thus, typical implementations of
multi-class logistic regression pick one of the classes as a reference.
However, the over-parametrization problem is solved by adding ridge
regularization, that is by considering the following objective function
\[
f(\beta) = \left[-\sum_{i=1}^n\left\{\sum_{k=0}^{K-1}1(y_i=k)\log p_{k}(x_i)\right\} + \frac{\lambda}2\sum_{k=0}^{K-1}\sum_{j=1}^p\beta_{k,j}^2\right], \quad \mbox{where} \quad  p_k(x_i)=\frac{e^{x_i^{\top}\beta_k}}{\sum_{l=0}^{K-1} e^{x_i^{\top}\beta_l}}.
\] with some \(\lambda >0\) over
\(\beta = (\beta_0, \dots, \beta_{K-1})\in \mathbb{R}^{p \times K}\).
Here \(1(y_i=k)\) is the indicator function, that is it is equal to one
when \(y_i=k\) and is equal to zero when \(y_i \neq k\). The ridge
regularization makes the solution unique as it essentially looks for
\(K\) vectors \(\beta_k\) that satisfy the above model **and have the
minimal euclidean norm**.

Thus, the objective function \(f(\beta)\) depends on the supplied
training data \(X\in \mathbb{R}^{n\times p}\) (with 1st column being all
ones to account for intercept) and \(y \in \{0, \dots, K-1\}^n\); and
its argument is matrix
\(\beta = (\beta_0, \dots, \beta_{K-1})\in \mathbb{R}^{p \times K}\),
where each column corresponds to specific \(\beta_k\).

We find the matrix \(\beta\) by minimizing the above \(f(\beta)\). Once
the minimizer \(\beta^* \in \mathbb{R}^{p \times K}\) is found, the
classification for a new \(x\in \mathbb{R}^{p}\) is performed by
assigning \(x\) to the class with the largest probability \(p_k(x)\),
where \[
p_k(x) = \frac{e^{x^{\top}\beta^*_k}}{\sum_{l=0}^{K-1}e^{x^{\top}\beta^*_l}}.
\]

## Damped Newton’s method implementation

In this assignment, we will implement Damped Newton’s method to minimize
\(f(\beta)\).

Let \(P_k = P_k(X; \beta) \in \mathbb{R}^n\) be a vector containing
class specific probabilities \(p_k(x_i)\) as defined above (depend on
\(\beta\)). Let \(W_k = W_k(X; \beta)\) be a \(n \times n\) diagonal
matrix with diagonal elements \(w_{kii}=p_k(x_i)(1-p_{k}(x_i))\) (depend
on \(\beta\) as \(p_k(x_i)\) depend on \(\beta\)). There are \(K\) total
vectors \(P_k\), and \(K\) total matrices \(W_k\). It can be shown that
the Damped Newton’s update with learning rate \(\eta >0\) for
\(f(\beta)\) has the form \[
\beta_k^{(t+1)} = \beta_k^{(t)} - \eta (X^{\top}W_kX + \lambda I)^{-1}\left[X^{\top}\left\{P_k - 1(Y = k) \right\} + \lambda \beta_k^{(t)}\right],\quad k=0,\dots, K-1;
\] where \(1(Y = k)\in \mathbb{R}^n\) is a vector with elements
\(1(y_i=k)\). Here both \(W_k\) and \(P_k\) depend on \(\beta^{(t)}\).

**Task 2:** **FunctionsLR.R** contains the wrapper for the following
function

## Description of the Data

  - **letter-train.txt**, **letter-test.txt** - we will use these data
    of 16 features to assign one of the 26 labels (corresponding to
    different letters in the alphabet, comes from images)

