# The correct function
Myellipse <- function(a, b)
{
	accept <- 0
	count <- 0
	prop <- numeric(length = 2)
	while(accept == 0)
	{
		count <- count + 1
		prop[1] <- runif(1, min = -sqrt(a), max = sqrt(a))
		prop[2] <- runif(1, min = -sqrt(b), max = sqrt(b))

		if(prop[1]^2/a + prop[2]^2/b <= 1)
		{
			accept <- 1
		}
	}
	return(c(prop, count))
}

# This is how I am checking
reps <- 1e3
samp <- matrix(0, nrow = reps, ncol = 3)
for(r in 1:reps)
{
	samp[r, ] <- Myellipse(4, .25)
}
mean(samp[ ,3])
plot(samp[, 1:2], type = "p", asp = 1)