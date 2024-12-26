############################
## Solution to Assignment 2
############################
# First solution
MYrmixNorm <- function(n, p, mu1, mu2, Sigma1, Sigma2)
{
  # find dimension
  k <- length(mu1)
  # the matrix that will be returned
  output <- matrix(0, nrow = n, ncol = k)

  # do the square-root calculations before the loop
  decomp1 <- eigen(Sigma1)
  decomp2 <- eigen(Sigma2)
  Sig.sq1 <- decomp1$vectors %*% diag(decomp1$values^(1/2)) %*% solve(decomp1$vectors)
  Sig.sq2 <- decomp2$vectors %*% diag(decomp2$values^(1/2)) %*% solve(decomp2$vectors)
  
  # generate uniform vector outside to save time
  Us <- runif(n)
  for(i in 1:n)
  {
    # you can try generating this outside
    # however for large n, storage will be an issue
    Z <- rnorm(k)

    if(Us[i] < p)
    {
      output[i, ] <- mu1 + Sig.sq1 %*% Z
    } else{
      output[i, ] <- mu2 + Sig.sq2 %*% Z
    }
  }
  return(output)
}


############################
## Alternate solution that is faster
## courtesy Yash Bihany
## but with his errors fixed
############################
MYrmixNorm2 <- function(n, p, mu1, mu2, Sigma1, Sigma2)
{
  # find dimension
  k <- length(mu1)
  # the matrix that will be returned
  output <- matrix(0, nrow = n, ncol = k)

  # do the square-root calculations before
  decomp1 <- eigen(Sigma1)
  decomp2 <- eigen(Sigma2)
  Sig.sq1 <- decomp1$vectors %*% diag(decomp1$values^(1/2)) %*% solve(decomp1$vectors)
  Sig.sq2 <- decomp2$vectors %*% diag(decomp2$values^(1/2)) %*% solve(decomp2$vectors)
  

  # Without any loops
  Us <- runif(n)
  Zs <- matrix(rnorm(n*k), nrow = n, ncol = k)
  ind <- Us <= p
  ind2 <- Us > p

  output[ind, ] <- t(mu1 + t(Zs[ind, ] %*% Sig.sq1))
  output[ind2, ] <- t(mu2 +  t(Zs[ind2, ] %*% Sig.sq2) )

  return(output)
}





# Uncomment and run to test both functions.
# mu1 <- c(2,4)
# mu2 <- c(-3, -1)
# Sigma1 <- matrix(c(4, -4, -4, 5), nrow = 2, ncol = 2)
# Sigma2 <- matrix(c(4, 4, 4, 5), nrow = 2, ncol = 2)

# samples <- MYrmixNorm(1e3, p = .3, mu1 = mu1, mu2 = mu2, 
#   Sigma1 = Sigma1, Sigma2 = Sigma2)

# plot(samples)
# cov(samples)

# library(rbenchmark)

# ## second one is much faster
# benchmark(MYrmixNorm(1e4, p = .3, mu1 = mu1, mu2 = mu2, 
#   Sigma1 = Sigma1, Sigma2 = Sigma2),
# MYrmixNorm2(1e4, p = .3, mu1 = mu1, mu2 = mu2, 
#   Sigma1 = Sigma1, Sigma2 = Sigma2))