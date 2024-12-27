### Paste all your codes for model building
### and cross-validation here
# Load the training data
a <- read.csv("assign4_train.csv")

# Prepare for cross-validation
b <- 5  # Splitting the data into 5 folds for cross-validation
c <- nrow(a)
d <- floor(c / b)

# Prepare to store errors for each fold
e <- numeric(b)

# Perform cross-validation
for (f in 1:b) {
  # Determine the indices for the current fold
  g <- (f - 1) * d + 1
  h <- min(g + d - 1, c)
  i <- g:h
  
  # Split the data into training and testing sets for this fold
  j <- a[-i, ]
  k <- a[i, ]
  
  # Fit a linear regression model on the training data
  l <- lm(y ~ ., data = j)
  
  # Predict on the test data
  m <- predict(l, newdata = k)
  
  # Calculate the mean squared error for this fold
  e[f] <- mean((k$y - m)^2)
}

# Calculate the average error across all folds
n <- mean(e)

# Save the model parameters
save(l, file = "fit_params.Rdata")
