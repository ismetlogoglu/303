
data <- scan("data set 14.txt")

data2 <- scan("data set 24.txt")
print(data)


summary(data)


df <- data.frame(values = data)
df


hist(data, prob=TRUE)
lines(density(data), col="red")

# Q-Q plot for exponential distribution
library(ggplot2)
ggplot(data.frame(x=data), aes(sample=x)) + 
  stat_qq(distribution=qexp) +
  stat_qq_line(distribution=qexp)
