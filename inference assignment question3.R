#Question 3.1
set.seed(48703186)
xmax <- sample(2:15, size = 1)
xmax

#Question 3.3
theta <- c(0.5, 1, 2, 5)
n <-  5000
samples <- list()
for( j in 1:length(theta)){
  U <- runif(n)
  x <- 12 * U^(1/theta[j]) #applying inverse cdf
  samples[[j]] <-  x
}
install.packages("ggplot2")
library(ggplot2)
expected_values <- 12*theta/ (theta+1)
expected_values

#histogram for theta = 0.5
ggplot(data.frame(x = samples[[1]]), aes(x = x)) +
  geom_histogram(bins = 40, fill = "#FFB4A2", color = "#9D4E36") +
  geom_vline(xintercept = 12*0.5/(0.5+1),
             color = "red", linewidth = 1) +
  labs(title = "Distribution for theta = 0.5",
       x = "X", y = "Frequency") +
  theme_minimal()
#the distribution is dense near 0 and decreases towards 12. It is right skewed.  

#histogram for theta = 1
ggplot(data.frame(x = samples[[2]]), aes(x = x)) +
  geom_histogram(bins = 40, fill = "#CDB4DB", color = "#6A4C93") +
  geom_vline(xintercept = 12*1/(1+1),
             color = "red", linewidth = 1) +
  labs(title = "Distribution for theta = 1",
       x = "X", y = "Frequency") +
  theme_minimal()
#the distribution is uniform throughout. The values are equal likely in the interval [0,12].

#histogram for theta = 2
ggplot(data.frame(x = samples[[3]]), aes(x = x)) +
  geom_histogram(bins = 40, fill = "#A3B18A", color = "#344E81") +
  geom_vline(xintercept = 12*2/(2+1),
             color = "red", linewidth = 1) +
  labs(title = "Distribution for theta = 2",
       x = "X", y = "Frequency") +
  theme_minimal()
#The distribution increases towards 12, with more observations near 12. Left Skewed.

#histogram for theta = 5
ggplot(data.frame(x = samples[[4]]), aes(x = x)) +
  geom_histogram(bins = 40, fill = "#BDE0FE", color = "#457B9D") +
  geom_vline(xintercept = 12*5/(5+1),
             color = "red", linewidth = 1) +
  labs(title = "Distribution for theta = 5",
       x = "X", y = "Frequency") +
  theme_minimal()
#Left skewed plot. strongly concentrated near 12 and very few observations near 0.

#Comment and Discussion
#The distribution changes with the value of θ. For θ = 0.5, observations are concentrated near zero, resulting in a right-skewed distribution. For θ = 1, the distribution becomes uniform. As θ increases to 2 and 5, observations become increasingly concentrated towards the upper limit (12). The theoretical expected value also increases with θ, moving from 4 to 10.

#Question 3.4
set.seed(48703186)
n <- 100
theta <-  1
U <- runif(n)
x <- 12 * U^(1/theta)
mle <- n / (n *log(12) - sum(log(x)))
mom <- mean(x) / (12 - mean(x))

mle
mom

#Both the mle and mom produces estimates closer to the true value of theta = 1. The MLE is 0.9318784 and MoM is 0.9371231. Both values are slightly lower than the true parameter value. The MoM estimator is a bit closer to the true value, however this single sample is insufficient to conclude that it is generally a better estimator.

#Question 3.5
set.seed(48703186)
theta <- 1
xmax <- 12
U <- runif(100)
x100 <- xmax * U^(1/theta)
U <- runif(10)
x10 <- xmax * U^(1/theta)
U <- runif(1000)
x1000 <- xmax * U^(1/theta)

mle10 <- 10/(10*log(xmax) - sum(log(x10)))
mle100 <- 100/(100*log(xmax) - sum(log(x100)))
mle1000 <- 1000/(1000*log(xmax) - sum(log(x1000)))

mom10 <- mean(x10)/(xmax - mean(x10))
mom100 <- mean(x100)/(xmax - mean(x100))
mom1000 <- mean(x1000)/(xmax - mean(x1000))

mle10
mom10

mle100
mom100

mle1000
mom1000

log_likelihood <- function(theta, x){
  n = length(x)
  n*log(theta) - n*theta*log(12) + (theta-1)*sum(log(x))
  
}

theta_values <- seq(0.05, 5, length.out = 500)

ll_values <- sapply(theta_values, log_likelihood, x = x10)

plot_data <- data.frame(theta = theta_values, loglik = ll_values)

ggplot(plot_data, aes(x = theta, y = loglik)) +
  geom_line() +
  geom_vline(aes(xintercept = mle10, color = "MLE"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = mom10, color = "MoM"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = 1, color = "True Value"),
             linetype = "dashed") +
  scale_color_manual(
    name = "Estimates",
    values = c("MLE" = "blue",
               "MoM" = "green",
               "True Value" = "red")
  ) +
  labs(title = "Log-Likelihood Function (n = 10)",
       x = "Theta",
       y = "Log-Likelihood") +
  theme_minimal() +
  theme(legend.position = "right")

theta_values <- seq(0.05, 5, length.out = 500)

ll_values <- sapply(theta_values, log_likelihood, x = x100)

plot_data <- data.frame(theta = theta_values, loglik = ll_values)

ggplot(plot_data, aes(x = theta, y = loglik)) +
  geom_line() +
  geom_vline(aes(xintercept = mle100, color = "MLE"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = mom100, color = "MoM"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = 1, color = "True Value"),
             linetype = "dashed") +
  scale_color_manual(
    name = "Estimates",
    values = c("MLE" = "blue",
               "MoM" = "green",
               "True Value" = "red")
  ) +
  labs(title = "Log-Likelihood Function (n = 100)",
       x = "Theta",
       y = "Log-Likelihood") +
  theme_minimal() +
  theme(legend.position = "right")

theta_values <- seq(0.05, 5, length.out = 500)

ll_values <- sapply(theta_values, log_likelihood, x = x1000)

plot_data <- data.frame(theta = theta_values, loglik = ll_values)

ggplot(plot_data, aes(x = theta, y = loglik)) +
  geom_line() +
  geom_vline(aes(xintercept = mle1000, color = "MLE"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = mom1000, color = "MoM"),
             linetype = "dashed") +
  geom_vline(aes(xintercept = 1, color = "True Value"),
             linetype = "dashed") +
  scale_color_manual(
    name = "Estimates",
    values = c("MLE" = "blue",
               "MoM" = "green",
               "True Value" = "red")
  ) +
  labs(title = "Log-Likelihood Function (n = 1000)",
       x = "Theta",
       y = "Log-Likelihood") +
  theme_minimal() +
  theme(legend.position = "right")

#Discussion
#The result shows the effect of sample size on the MLE and mom. 
#for n = 10. both the estimators are greater than the true parameter value (1), although they are very close to each other. The shows that estimates obtained from smaller samples may deviate considerably from the true parameter value.
#For n = 100, both estimators are very close to the true parameter value (1). The difference between the two estimates is also smaller.
#For n = 1000, both estimators are very close to the true parameter value (1). The difference between the two estimates is also relatively small . Moreover the the estimates are larger than the true value now. 
#The log likelihood function curve becomes increasingly peaked as n increases. This shows that the likelihood is more dense around MLE, which shows a better precision in estimation.
#Overall both the MLE and MoM estimate better with increasing n. Also the MLE is always located at the maximum of log likelihood curve, whereas the MoM does not necessarily coincide with that maximum.

#Question 3.6
d <-  qchisq(0.95, df = 1)
d
plot_wilks <-  function(x, sample_size){
  n <-  length(x)
  mle <- n / (n *log(12) - sum(log(x)))
  max_ll <- log_likelihood(mle, x)
  cutoff <-  max_ll - d/2
  #find lower and upper confidence limits
  lower <- uniroot(
    function(theta) log_likelihood(theta, x) - cutoff,
    interval = c(0.000001, mle)
  )$root
  
  upper <- uniroot(
    function(theta) log_likelihood(theta, x) - cutoff,
    interval = c(mle, 5)
  )$root
  
  theta_values <- seq(0.001, 5, length.out = 1000)
  
  ll_values <- sapply(theta_values, log_likelihood, x = x)
  
  plot_data <- data.frame(
    theta = theta_values,
    loglik = ll_values
  )
  
  print(
    ggplot(plot_data, aes(x = theta, y = loglik)) +
      geom_line() +
      geom_hline(yintercept = cutoff,
                 color = "red", linetype = "dashed") +
      geom_vline(xintercept = lower,
                 color = "blue", linetype = "dashed") +
      geom_vline(xintercept = upper,
                 color = "blue", linetype = "dashed") +
      geom_vline(xintercept = mle,
                 color = "darkgreen", linetype = "dotted") +
      labs(
        title = paste("Wilks Confidence Interval (n =", sample_size, ")"),
        x = "Theta",
        y = "Log-Likelihood"
      ) +
      theme_minimal()
  )
  cat("Sample size:", sample_size, "\n")
  cat("MLE:", mle, "\n")
  cat("Lower limit:", lower, "\n")
  cat("Upper limit:", upper, "\n\n")
  
}
plot_wilks(x10, 10)

plot_wilks(x100, 100)

plot_wilks(x1000, 1000)


#The Wilks confidence interval plots shows the relation between sample size and precision of parameter estimation.
#for n = 10, the CI is relatively wide, showing uncertainty in estimating the parameter. As the sample size increases to 100, the interval becomes narrower, showing improved precision. 
#At n = 1000, the CI becomes more narrow, showing even greater precision. The true parameter theta = 1, lies within all 3 CIs.
#Overall the result shows that the increasing sample size lowers the width of the CI and increases estimation precision . This is consistent with the expectation that larger samples provide more information about the unknown parameter.


#Question 3.7
negative_log_likelihood <- function(theta, x) {
  if(theta <= 0) return(1e100)
  -log_likelihood(theta, x)
}
result10 <- nlm(negative_log_likelihood, p = 1,
                x = x10, hessian = TRUE)
result100 <- nlm(negative_log_likelihood, p = 1,
                 x = x100, hessian = TRUE)
result1000 <- nlm(negative_log_likelihood, p = 1,
                  x = x1000, hessian = TRUE)
mle_nlm10 <- result10$estimate
mle_nlm100 <- result100$estimate
mle_nlm1000 <- result1000$estimate
se10 <- sqrt(1/as.numeric(result10$hessian))
se100 <- sqrt(1/as.numeric(result100$hessian))
se1000 <- sqrt(1/as.numeric(result1000$hessian))
CI10 <- mle_nlm10 + c(-1, 1)*1.96*se10
CI100 <- mle_nlm100 + c(-1, 1)*1.96*se100
CI1000 <- mle_nlm1000 + c(-1, 1)*1.96*se1000
d <- qchisq(0.95, df = 1)
lower10 <- uniroot(function(t)
  log_likelihood(t, x10) -
    (log_likelihood(mle_nlm10, x10) - d/2),
  interval = c(0.000001, mle_nlm10))$root
upper10 <- uniroot(function(t)
  log_likelihood(t, x10) -
    (log_likelihood(mle_nlm10, x10) - d/2),
  interval = c(mle_nlm10, 5))$root
lower100 <- uniroot(function(t)
  log_likelihood(t, x100) -
    (log_likelihood(mle_nlm100, x100) - d/2),
  interval = c(0.000001, mle_nlm100))$root
upper100 <- uniroot(function(t)
  log_likelihood(t, x100) -
    (log_likelihood(mle_nlm100, x100) - d/2),
  interval = c(mle_nlm100, 5))$root
lower1000 <- uniroot(function(t)
  log_likelihood(t, x1000) -
    (log_likelihood(mle_nlm1000, x1000) - d/2),
  interval = c(0.000001, mle_nlm1000))$root
upper1000 <- uniroot(function(t)
  log_likelihood(t, x1000) -
    (log_likelihood(mle_nlm1000, x1000) - d/2),
  interval = c(mle_nlm1000, 5))$root
# Comparison table
comparison <- data.frame(
  Sample_Size = c(10, 100, 1000),
  Numerical_MLE = c(mle_nlm10, mle_nlm100, mle_nlm1000),
  Wilks_Lower = c(lower10, lower100, lower1000),
  Wilks_Upper = c(upper10, upper100, upper1000),
  Wald_Lower = c(CI10[1], CI100[1], CI1000[1]),
  Wald_Upper = c(CI10[2], CI100[2], CI1000[2])
)

comparison

#The results show that the wald and wilk ci become increasingly similar as the smaple size increases.
#fir n = 10, there is a significant difference between the two intervals. The wald interval is wider and has a lower lower bound than the wilks interval. 
#the difference occurs because the wald method uses a quadratic approximation of the log likelihood, which may be less accurate for the smaller samples.
#for n = 100 , the tow intervals are closer. for n = 1000, they are almost same, showing that both the methods provide similar estimates for larger samples.
#the width of both confidence intervals decrease as the sample size increases, showing improved precision in parameter estimation. All 6 CI contain the true parameter vale , theta = 1.
#Overall the result shows that the wald and wilks methods become asymptotially equivalent as the sample size increases.

#Question 3.8
set.seed(48703186)
theta <- 1
xmax <- 12
negative_log_likelihood <- function(theta, x) {
  if(theta <= 0) return(1e100)
  -log_likelihood(theta, x)
}
one_simulation <- function(n) {
  x <- xmax * runif(n)^(1/theta)
  result <- nlm(negative_log_likelihood, p=1, x=x, hessian=TRUE)
  mle <- result$estimate
  se <- sqrt(1/as.numeric(result$hessian))
  lower <- mle - 1.96 * se
  upper <- mle + 1.96 * se
  mom <- mean(x)/(xmax-mean(x))
  coverage <- (lower <= theta && theta <= upper)
  return(c(MLE=mle, MoM=mom,
           Lower=lower, Upper=upper,
           Coverage=coverage))
}
n <- c(10, 100, 1000)
results <- list()
for(j in 1:length(n)) {
  simulations <- replicate(1000, one_simulation(n[j]))
  results[[j]] <- as.data.frame(t(simulations))
}
summary_results <- data.frame(
  Sample_Size = n,
  Mean_MLE = sapply(results, function(x) mean(x$MLE)),
  Mean_MoM = sapply(results, function(x) mean(x$MoM)),
  MLE_Bias = sapply(results, function(x) mean(x$MLE) - theta),
  MoM_Bias = sapply(results, function(x) mean(x$MoM) - theta),
  Wald_Coverage = sapply(results, function(x) mean(x$Coverage))
)
summary_results

#The simulation results show that both MLE and MoM estimators approach the true parameter value (theta=1) as the sample size increases. The bias of both estimators decreases considerably, indicating improved estimation accuracy with larger samples. In these simulations, the MoM estimator has slightly lower bias than the MLE at all three sample sizes.
#The coverage probabilities of the 95% Wald confidence intervals are 95.9%, 95.1%, and 94.1% for sample sizes 10, 100, and 1000, respectively. These values are close to the expected 95% coverage. Overall, the results are consistent with expectations: both estimators become more accurate with increasing sample size, while the Wald confidence intervals maintain coverage close to their nominal level.

