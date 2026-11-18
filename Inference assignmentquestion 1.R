#Question 1

#1.1
mle_exp <- function(lambda, n){
  x <- rexp(n, rate = lambda)
  mle <-  1/mean(x)
  return(mle)
}

#1.2
set.seed(48703186) #sum of 26346699 (Sumit) and 22356487(Kallen)

mle_exp <- function(lambda, n){
  x <- rexp(n, rate = lambda)
  mle <-  1/mean(x)
  return(mle)
}

mle_exp(1, 10)


#1.3
set.seed(48703186)

mle_values <- numeric(1000)
for(i in 1:1000){
  mle_values[i] <- mle_exp(1,10)
}
mean <- mean(mle_values)
variance <- var(mle_values)
mse <- mean((mle_values - 1)^2)

mean
variance
mse

#1.4
#bias vs sample size graph
sample_size <- c(10,20,50,200,400,1000)
bias_values <- numeric(length(sample_size))
for(j in 1: length(sample_size)){
  set.seed(48703186)
  mle_values <-  numeric(1000)
  for(i in 1:1000){
    mle_values[i] <- mle_exp(1, sample_size[j])
  }
  bias_values[j] <-  mean(mle_values) - 1
}

bias_values
analytical_bias <- c(1/9, 1/19, 1/49, 1/199, 1/399, 1/999)
plot(sample_size, bias_values, 
     type = 'b',
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Simulated Bias",
     main = "Bias vs Sample Size")
lines(sample_size, analytical_bias,
      type = "b",
      pch = 17,
      col = "red",
      lty = 2)
legend("topright",
       legend = c("Simulated Bias", "Analytical Bias"),
       col = c("black", "red"),
       pch = c(16, 17),
       lty = c(1, 2))

# Efficiency vs Sample Size

sample_size <- c(10, 20, 50, 200, 400, 1000)

efficiency_values <- numeric(length(sample_size))

for(j in 1:length(sample_size)){
  
  set.seed(48703186)
  
  mle_values <- numeric(1000)
  
  for(i in 1:1000){
    mle_values[i] <- mle_exp(1, sample_size[j])
  }
  
  variance <- var(mle_values)
  crlb <- 1/sample_size[j]
  
  efficiency_values[j] <- crlb/variance
}

efficiency_values

analytical_efficiency <- c(
  (9^2 * 8)/10^3,
  (19^2 * 18)/20^3,
  (49^2 * 48)/50^3,
  (199^2 * 198)/200^3,
  (399^2 * 398)/400^3,
  (999^2 * 998)/1000^3
)

plot(sample_size, efficiency_values,
     type = "b",
     pch = 16,
     ylim = c(0,1),
     xlab = "Sample Size (n)",
     ylab = "Efficiency",
     main = "Efficiency vs Sample Size")

lines(sample_size, analytical_efficiency,
      type = "b",
      pch = 17,
      col = "red",
      lty = 2)

legend("bottomright",
       legend = c("Simulated Efficiency", "Analytical Efficiency"),
       col = c("black", "red"),
       pch = c(16, 17),
       lty = c(1, 2))

#MSE vs sample size

sample_size <- c(10, 20, 50, 200, 400, 1000)

mse_values <- numeric(length(sample_size))

for(j in 1:length(sample_size)){
  
  set.seed(48703186)
  
  mle_values <- numeric(1000)
  
  for(i in 1:1000){
    mle_values[i] <- mle_exp(1, sample_size[j])
  }
  
  mse_values[j] <- mean((mle_values - 1)^2)
}

mse_values

analytical_mse <- c(
  12/(9*8),
  22/(19*18),
  52/(49*48),
  202/(199*198),
  402/(399*398),
  1002/(999*998)
)

plot(sample_size, mse_values,
     type = "b",
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Mean Squared Error",
     main = "MSE vs Sample Size")

lines(sample_size, analytical_mse,
      type = "b",
      pch = 17,
      col = "red",
      lty = 2)

legend("topright",
       legend = c("Simulated MSE", "Analytical MSE"),
       col = c("black", "red"),
       pch = c(16, 17),
       lty = c(1, 2))

#1.5

#lambda = 0.5  
lambda <- 0.5  

sample_size <- c(10, 20, 50, 200, 400, 1000)

bias_values <- numeric(length(sample_size))
efficiency_values <- numeric(length(sample_size))
mse_values <- numeric(length(sample_size))

for(j in 1:length(sample_size)){
  
  set.seed(48703186)
  
  mle_values <- numeric(1000)
  
  for(i in 1:1000){
    mle_values[i] <- mle_exp(lambda, sample_size[j])
  }

  bias_values[j] <- mean(mle_values) - lambda

  variance <- var(mle_values)
  crlb <- lambda^2/sample_size[j]
  efficiency_values[j] <- crlb/variance

  mse_values[j] <- mean((mle_values - lambda)^2)
}

analytical_bias <- lambda/(sample_size - 1)

analytical_efficiency <- 
  ((sample_size - 1)^2 * (sample_size - 2))/sample_size^3

analytical_mse <- 
  (lambda^2 * (sample_size + 2))/
  ((sample_size - 1) * (sample_size - 2))


# Bias vs Sample Size (lambda = 0.5 )

plot(sample_size, bias_values,
     type = "b",
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Bias",
     main = paste("Bias vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_bias,
      type = "b", pch = 17, col = "red", lty = 2)

legend("topright",
       legend = c("Simulated Bias", "Analytical Bias"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))


# Efficiency vs Sample Size (lambda = 0.5 )

plot(sample_size, efficiency_values,
     type = "b",
     pch = 16,
     ylim = c(0, 1),
     xlab = "Sample Size (n)",
     ylab = "Efficiency",
     main = paste("Efficiency vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_efficiency,
      type = "b", pch = 17, col = "red", lty = 2)

legend("bottomright",
       legend = c("Simulated Efficiency", "Analytical Efficiency"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))


# MSE vs Sample Size (lambda = 0.5 )

plot(sample_size, mse_values,
     type = "b",
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Mean Squared Error",
     main = paste("MSE vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_mse,
      type = "b", pch = 17, col = "red", lty = 2)

legend("topright",
       legend = c("Simulated MSE", "Analytical MSE"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))

# lambda = 2
lambda <- 2 

sample_size <- c(10, 20, 50, 200, 400, 1000)

bias_values <- numeric(length(sample_size))
efficiency_values <- numeric(length(sample_size))
mse_values <- numeric(length(sample_size))

for(j in 1:length(sample_size)){
  
  set.seed(48703186)
  
  mle_values <- numeric(1000)
  
  for(i in 1:1000){
    mle_values[i] <- mle_exp(lambda, sample_size[j])
  }
  
  bias_values[j] <- mean(mle_values) - lambda
  
  variance <- var(mle_values)
  crlb <- lambda^2/sample_size[j]
  efficiency_values[j] <- crlb/variance
  
  mse_values[j] <- mean((mle_values - lambda)^2)
}

analytical_bias <- lambda/(sample_size - 1)

analytical_efficiency <- 
  ((sample_size - 1)^2 * (sample_size - 2))/sample_size^3

analytical_mse <- 
  (lambda^2 * (sample_size + 2))/
  ((sample_size - 1) * (sample_size - 2))


# Bias vs Sample Size (lambda = 2)

plot(sample_size, bias_values,
     type = "b",
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Bias",
     main = paste("Bias vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_bias,
      type = "b", pch = 17, col = "red", lty = 2)

legend("topright",
       legend = c("Simulated Bias", "Analytical Bias"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))


# Efficiency vs Sample Size (lambda = 2)

plot(sample_size, efficiency_values,
     type = "b",
     pch = 16,
     ylim = c(0, 1),
     xlab = "Sample Size (n)",
     ylab = "Efficiency",
     main = paste("Efficiency vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_efficiency,
      type = "b", pch = 17, col = "red", lty = 2)

legend("bottomright",
       legend = c("Simulated Efficiency", "Analytical Efficiency"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))


# MSE vs Sample Size (lambda = 2)

plot(sample_size, mse_values,
     type = "b",
     pch = 16,
     xlab = "Sample Size (n)",
     ylab = "Mean Squared Error",
     main = paste("MSE vs Sample Size (lambda =", lambda, ")"))

lines(sample_size, analytical_mse,
      type = "b", pch = 17, col = "red", lty = 2)

legend("topright",
       legend = c("Simulated MSE", "Analytical MSE"),
       col = c("black", "red"),
       pch = c(16, 17), lty = c(1, 2))


## Discussion of Results

#The results show the effect of n and lambda on the MLE of the exp distribution. For all 3 lambda values (0.5, 1, 2), the bias and mse decreases as n increases. The estimator becomes more accurate and approaches towards unbiasness as n increases.This shows the practical example of CLT.
#Lambda is directly proportional to Bias and MSE is proportional to Lambda^2.As efficiency does not depends on lambda , it was found identical for all 3 parameter values and approaches to 1 as n increases and becomes asymptotically efficient.
#Sometimes very small differences occur in simulated results and analytical results due to random sampling variation and finite number of replications (1000).Overall increasing n, improves the performance of MLE, while changing lambda affects its bias and MSE but not its efficiency.