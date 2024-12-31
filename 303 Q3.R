
data <- scan("data set 14.txt")


print(data)


summary(data)


df <- data.frame(values = data)
df

#dataset 14

hist(data, prob=TRUE)
lines(density(data), col="red")

# Q-Q plot for exponential distribution
library(ggplot2)
ggplot(data.frame(x=data), aes(sample=x)) + 
  stat_qq(distribution=qexp) +
  stat_qq_line(distribution=qexp)




# Function to calculate UMVU estimator for exponential distribution
umvu_estimator <- function(data) {
  n <- length(data)
  x_bar <- mean(data)
  lambda_umvu <- (n-1)/(n*x_bar)
  return(lambda_umvu)
}
cat("UMVU Estimator (λ̂):", lambda_umvu, "\n")



# Function to calculate MME estimator for exponential distribution
mme_estimator <- function(data) {
  x_bar <- mean(data)
  lambda_mme <- 1/x_bar
  return(lambda_mme)
}
cat("MME Estimator (λ̂):", lambda_mme, "\n")


# Calculate estimators
lambda_umvu <- umvu_estimator(data)
lambda_mme <- mme_estimator(data)

# Choose 'a' as the mean of the data
a <- mean(data)

# Calculate probabilities using UMVU estimator
p_greater_umvu <- exp(-lambda_umvu * a)
p_less_umvu <- 1 - exp(-lambda_umvu * a)

# Calculate probabilities using MME estimator
p_greater_mme <- exp(-lambda_mme * a)
p_less_mme <- 1 - exp(-lambda_mme * a)

# Print results
cat("Results for a =", a, "\n\n")
cat("UMVU Estimator (λ̂):", lambda_umvu, "\n")
cat("P(X > a) using UMVU:", p_greater_umvu, "\n")
cat("P(X < a) using UMVU:", p_less_umvu, "\n\n")
cat("MME Estimator (λ̂):", lambda_mme, "\n")
cat("P(X > a) using MME:", p_greater_mme, "\n")
cat("P(X < a) using MME:", p_less_mme, "\n")

# Empirical probabilities for comparison
emp_greater <- mean(data > a)
emp_less <- mean(data < a)
cat("\nEmpirical Probabilities:\n")
cat("P(X > a):", emp_greater, "\n")
cat("P(X < a):", emp_less, "\n")




#dataset24

data2 <- scan("data set 24.txt")


# Histogram and density
hist(data2, prob=TRUE)
lines(density(data2), col="blue4")

# Q-Q plot for exponential distribution

ggplot(data.frame(x=data2), aes(sample=x)) + 
  stat_qq(distribution=qexp) +
  stat_qq_line(distribution=qexp)



# Function to calculate UMVU estimator for exponential distribution
umvu_estimator <- function(data2) {
  n <- length(data)
  x_bar <- mean(data)
  lambda_umvu <- (n-1)/(n*x_bar)
  return(lambda_umvu)
}

# Function to calculate MME estimator for exponential distribution
mme_estimator <- function(data2) {
  x_bar <- mean(data)
  lambda_mme <- 1/x_bar
  return(lambda_mme)
}



# Calculate estimators
lambda_umvu <- umvu_estimator(data2)
lambda_mme <- mme_estimator(data2)

# Choose 'a' as the mean of the data
a <- mean(data)

# Calculate probabilities using UMVU estimator
p_greater_umvu <- exp(-lambda_umvu * a)
p_less_umvu <- 1 - exp(-lambda_umvu * a)

# Calculate probabilities using MME estimator
p_greater_mme <- exp(-lambda_mme * a)
p_less_mme <- 1 - exp(-lambda_mme * a)

# Print results
cat("Results for a =", a, "\n\n")
cat("UMVU Estimator (λ̂):", lambda_umvu, "\n")
cat("P(X > a) using UMVU:", p_greater_umvu, "\n")
cat("P(X < a) using UMVU:", p_less_umvu, "\n\n")
cat("MME Estimator (λ̂):", lambda_mme, "\n")
cat("P(X > a) using MME:", p_greater_mme, "\n")
cat("P(X < a) using MME:", p_less_mme, "\n")

# Empirical probabilities for comparison
emp_greater <- mean(data > a)
emp_less <- mean(data < a)
cat("\nEmpirical Probabilities:\n")
cat("P(X > a):", emp_greater, "\n")
cat("P(X < a):", emp_less, "\n")










