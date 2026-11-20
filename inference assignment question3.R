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