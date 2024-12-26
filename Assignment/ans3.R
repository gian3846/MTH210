f.gradient <- function(y, X, beta, lam)
{
     # converting beta to compatible matrix form
     beta <- matrix(beta, ncol = 1)
     pi.vec <-  1 / (1 + exp(-X%*%beta/2))
     rtn <- colSums(X* as.numeric(y - pi.vec)/2) - lam*beta
     return(rtn)
}

f.hessian <- function(y, X, beta, lam)
   {
     beta <- matrix(beta, ncol = 1)
     W_i <-  exp(X%*%beta/2) / (1 + exp(X%*%beta/2))^2
     W <- diag(as.numeric(W_i))
     rtn <- - t(X) %*% W %*% X/4 - diag(lam, dim(X)[2])
}

MYlogridge <- function(y, X, lam)
{

	p <- dim(X)[2]
	n <- length(y)

   tol <- 1e-10
   compare <- 100
   iter <- 1
   # starting from the zero-vector
   grad.vec <- c() # will store gradients here
   beta.current <- rep(0, p)
   beta.new <- beta.current
	while(compare > tol)
	   {
	     iter <- iter + 1  # tracking iterations
	     gradient <- f.gradient(y, X, beta.current, lam)
	     hessian <- f.hessian(y, X, beta.current, lam)
	     beta.new <- beta.current - qr.solve(hessian) %*% gradient
	     grad.vec[iter] <- norm(gradient, "2")
	     beta.current <- beta.new
	     compare <- grad.vec[iter]
	}
	return(beta.current)
}


titanic <- read.csv("https://dvats.github.io/assets/titanic.csv")
head(titanic)

y <- titanic$Survived
X <- as.matrix(titanic[, -1]) # everything but the first column is the X
MYlogridge(y, X, lam = 2)

