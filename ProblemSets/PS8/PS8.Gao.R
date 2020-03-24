```{r}
library(nloptr)

set.seed(100)

N <- 100000
K <- 10
sigma <- 0.5

X <- matrix(rnorm(N*K,mean=0,sd=sigma), N, K)
X[,1] <- 1
eps <- rnorm(N,mean=0,sd=sigma)

beta <- c(1.5,-1,-.25,.75,3.5,-2,.5,1,1.25,2)

y <- X%*% beta + eps

beta.hat.OLS <- solve(t(X)%*%X)%*%(t(X)%*%y)


gradient <- function(beta,y,X) {
  return(as.vector(-2*t(X)%*%(y-X%*%beta)))
}

# set up a stepsize
alpha <- 0.0000003
# set up a number of iteration
iter <- 1

# define the gradient 
gradient <- function(beta,y,X) {
  return(as.vector(-2*t(X)%*%(y-X%*%beta)))
}
# randomly initialize a value to x
set.seed(100)
beta <- runif(dim(X)[2])

# create a matrix to contain all xs for all steps
beta.All <- matrix("numeric",length(beta),iter)


# gradient descent method to find the minimum
beta0 <- 0*beta.hat.matrix
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8){
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  if (iter%%10000==0) { print(beta)}
  iter <- iter+1}

# print result and plot all xs for every iteration
print(paste("The minimum of function(beta,y,X) is ", beta, sep = ""))
```

```{r}
library(nloptr)
## Our objective function
objfun <- function(beta,y,X) {
return (sum((y-X%*%beta)^2))
# equivalently, if we want to use matrix algebra:
# return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,y,X) {
return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
beta0
## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
beta.hat.LBFGS<-result$solution
beta.hat.LBFGS
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
beta.hat.NM<-result$solution
beta.hat.NM


## Check solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))
```
```{r}
library(nloptr)
## Our objective function
objfun  <- function(theta,y,X) {
# need to slice our parameter vector into beta and sigma components
beta    <- theta[1:(length(theta)-1)]
sig     <- theta[length(theta)]
# write objective function as *negative* log likelihood (since NLOPT minimizes)
loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
return (loglike)
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris))$coefficients[,1]),runif(1))

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,y=y,X=X)
print(result)
beta.hat.MLE <- result$solution

betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

## Check solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))


cbind(beta, beta.hat.OLS, beta.hat.LBFGS,beta.hat.NM,beta.hat.MLE)
```

```{r}
beta.hat.easy <- lm(Y ~ X -1)
beta.hat.easy

library(stargazer)
stargazer(beta.hat.OLS,beta.All,beta.hat.easy)
```

