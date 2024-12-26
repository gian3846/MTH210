# The correct function using Cauchy proposal
# Scaled Cauchy is also correct with different point of maxima
Mytdist <- function(v)
{
	# Proposal will be Cauchy
	# f(x)/g(x) is maximized at x = +-1 

	accept <- 0
	try <- 0

	logc <- dt(1, df = v, log = TRUE) - dcauchy(1, log = TRUE)

	while(accept == 0)
	{
		try <- try + 1
		U <- runif(1)

		propU <- runif(1)
		prop <- tan(pi*(propU - .5))

		log.ratio <- dt(prop, df = v, log = TRUE) - dcauchy(prop, log = TRUE) - logc

		if(log(U) < log.ratio)
		{
			return(c(prop, try))
		}
	}
}

# Testing
# v <- 10.5
# x <- seq(-20, 20, length = 500)
# out1 <- replicate(1e3, Mytdist(v))

# plot(density(out1[1, ]), col = "blue")
# lines(x, dt(x, df = v))


