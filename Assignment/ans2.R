### Correct function
normIS <- function(N)
{
	# Box-Muller
	R2 <- -2*log(runif(N))
	theta <- runif(N, min = 0, max = 2*pi)
	
	# N(0,1)
	samp <- sqrt(R2) * cos(theta)

	# N(0, 20)
	samp <- sqrt(20)*samp

	func <- samp * dnorm(samp, sd = sqrt(10))/dnorm(samp, sd = sqrt(20))
	est <- mean(func)
	variance <- var(func)

	return(c(est,variance))
}

# How I called it
normIS(N = 1e3)