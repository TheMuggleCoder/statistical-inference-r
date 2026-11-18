#Question 2
#Question 2.1

n <- c(5, 20, 50)
means <- list()
set.seed(48703186)
for(j in 1:length(n)){
  m <- numeric(1000)
  for(i in 1:1000){
    x <- rnorm(n[j], mean = 1, sd = 1)
    m[i] <- mean(x)
  }
  means[[j]] <- m
}

#Histogram 
#n = 5
install.packages("ggplot2")
library(ggplot2)
ggplot(data.frame(x = means[[1]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = '#FFB4A2', color = '#9D4E36') +
  labs(title = "(Histogram for n = 5 (rnorm(1,1))",
       x = "Sample mean",
       y = "Frequency") +
  theme_minimal()
#n = 20 
install.packages("ggplot2")
library(ggplot2)
ggplot(data.frame(x = means[[2]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = '#CDB4DB', color = '#6A4C93') +
  labs(title = "Histogram for n = 20 (rnorm(1,1))",
       x = "Sample mean",
       y = "Frequency") +
  theme_minimal()
#n = 50
install.packages("ggplot2")
library(ggplot2)
ggplot(data.frame(x = means[[3]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = '#A3B18A', color = '#344E81') +
  labs(title = "Histogram for n = 50 (rnorm(1,1))",
       x = "Sample mean",
       y = "Frequency") +
  theme_minimal()

#q-q plot
#n = 5
ggplot(data.frame(x = means[[1]]), aes(sample = x)) +
  stat_qq(color = "#FFB4A2", size = 2) +
  stat_qq_line(color = "#9D4E36", linewidth = 1) +
  labs(title = "Q-Q Plot for n = 5 (rnorm(1,1))",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data.frame(x = means[[2]]), aes(sample = x)) +
  stat_qq(color = "#CDB4DB", size = 2) +
  stat_qq_line(color = "#6A4C93", linewidth = 1) +
  labs(title = "Q-Q Plot for n = 20 (rnorm(1,1))",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data.frame(x = means[[3]]), aes(sample = x)) +
  stat_qq(color = "#A3B18A", size = 2) +
  stat_qq_line(color = "#344E81", linewidth = 1) +
  labs(title = "Q-Q Plot for n = 50 (rnorm(1,1))",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#Question 2.2
#exponential distribution with lambda = 1
n <- c(5, 20, 50)
set.seed(48703186)
means_exp <- list()
for(j in 1:length(n)){
  m <- numeric(1000)
  for(i in 1:1000){
    x <- rexp(n[j], rate = 1)
    m[i] <- mean(x)
  }
  means_exp[[j]] <- m
}

library(ggplot2)

# Histogram for n = 5
ggplot(data.frame(x = means_exp[[1]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#FFB4A2", color = "#9D4E36") +
  labs(title = "Exponential: Histogram for n = 5",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 20
ggplot(data.frame(x = means_exp[[2]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#CDB4DB", color = "#6A4C93") +
  labs(title = "Exponential: Histogram for n = 20",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 50
ggplot(data.frame(x = means_exp[[3]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#A3B18A", color = "#344E81") +
  labs(title = "Exponential: Histogram for n = 50",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Q-Q plot for n = 5

ggplot(data.frame(x = means_exp[[1]]), aes(sample = x)) +
  stat_qq(color = "#FFB4A2", size = 2) +
  stat_qq_line(color = "#9D4E36", linewidth = 1) +
  labs(title = "Exponential: Q-Q Plot for n = 5",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 20
ggplot(data.frame(x = means_exp[[2]]), aes(sample = x)) +
  stat_qq(color = "#CDB4DB", size = 2) +
  stat_qq_line(color = "#6A4C93", linewidth = 1) +
  labs(title = "Exponential: Q-Q Plot for n = 20",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 50
ggplot(data.frame(x = means_exp[[3]]), aes(sample = x)) +
  stat_qq(color = "#A3B18A", size = 2) +
  stat_qq_line(color = "#344E81", linewidth = 1) +
  labs(title = "Exponential: Q-Q Plot for n = 50",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


# Bernoulli distribution (p = 0.5)
set.seed(48703186)
n <- c(5, 20, 50)
means_bern05 <- list()
for(j in 1:length(n)){
  m <- numeric(1000)
  for(i in 1:1000){
    x <- rbinom(n[j], size = 1, prob = 0.5)
    m[i] <- mean(x)
  }
  means_bern05[[j]] <- m
}

# Histogram for n = 5
ggplot(data.frame(x = means_bern05[[1]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#FFB4A2", color = "#9D4E36") +
  labs(title = "Bernoulli (p=0.5): Histogram for n = 5",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 20
ggplot(data.frame(x = means_bern05[[2]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#CDB4DB", color = "#6A4C93") +
  labs(title = "Bernoulli (p=0.5): Histogram for n = 20",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 50
ggplot(data.frame(x = means_bern05[[3]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#A3B18A", color = "#344E81") +
  labs(title = "Bernoulli (p=0.5): Histogram for n = 50",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Q-Q plot for n = 5
ggplot(data.frame(x = means_bern05[[1]]), aes(sample = x)) +
  stat_qq(color = "#FFB4A2", size = 2) +
  stat_qq_line(color = "#9D4E36", linewidth = 1) +
  labs(title = "Bernoulli (p=0.5): Q-Q Plot for n = 5",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 20
ggplot(data.frame(x = means_bern05[[2]]), aes(sample = x)) +
  stat_qq(color = "#CDB4DB", size = 2) +
  stat_qq_line(color = "#6A4C93", linewidth = 1) +
  labs(title = "Bernoulli (p=0.5): Q-Q Plot for n = 20",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 50
ggplot(data.frame(x = means_bern05[[3]]), aes(sample = x)) +
  stat_qq(color = "#A3B18A", size = 2) +
  stat_qq_line(color = "#344E81", linewidth = 1) +
  labs(title = "Bernoulli (p=0.5): Q-Q Plot for n = 50",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


# Bernoulli distribution (p = 0.05)

n <- c(5, 20, 50)
means_bern005 <- list()
for(j in 1:length(n)){
  set.seed(48703186)
  m <- numeric(1000)
  for(i in 1:1000){
    x <- rbinom(n[j], size = 1, prob = 0.05)
    m[i] <- mean(x)
  }
  means_bern005[[j]] <- m
}

# Histogram for n = 5
ggplot(data.frame(x = means_bern005[[1]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#FFB4A2", color = "#9D4E36") +
  labs(title = "Bernoulli (p=0.05): Histogram for n = 5",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 20
ggplot(data.frame(x = means_bern005[[2]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#CDB4DB", color = "#6A4C93") +
  labs(title = "Bernoulli (p=0.05): Histogram for n = 20",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Histogram for n = 50
ggplot(data.frame(x = means_bern005[[3]]), aes(x = x)) +
  geom_histogram(bins = 25, fill = "#A3B18A", color = "#344E81") +
  labs(title = "Bernoulli (p=0.05): Histogram for n = 50",
       x = "Sample Means", y = "Frequency") +
  theme_minimal()

# Q-Q plot for n = 5
ggplot(data.frame(x = means_bern005[[1]]), aes(sample = x)) +
  stat_qq(color = "#FFB4A2", size = 2) +
  stat_qq_line(color = "#9D4E36", linewidth = 1) +
  labs(title = "Bernoulli (p=0.05): Q-Q Plot for n = 5",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 20
ggplot(data.frame(x = means_bern005[[2]]), aes(sample = x)) +
  stat_qq(color = "#CDB4DB", size = 2) +
  stat_qq_line(color = "#6A4C93", size = 2) +
  labs(title = "Bernoulli (p=0.05): Q-Q Plot for n = 20",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# Q-Q plot for n = 50
ggplot(data.frame(x = means_bern005[[3]]), aes(sample = x)) +
  stat_qq(color = "#A3B18A", size = 2) +
  stat_qq_line(color = "#344E81", linewidth = 1) +
  labs(title = "Bernoulli (p=0.05): Q-Q Plot for n = 50",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#Comment on the distribution of the sample m
#The distribution of the sample mean depends on the underlying population distribution and sample size.
#Exponential Distribution (lambda = 1):
  #for n = 5 the distribution is right skewed and does not look like a normal distribution. however as the sample size increases 20 and then 50 , the distribution becomes more symmetric and approximately normal . This show the Central Limit Theorem that as the n gets bigger, the distribution tends towards normal distribution.
#Bernoulli Distribution  (p = 0.50):
  #for n = 5, the plot is symmetric. However unlike normal data, the bernoulli data is discrete. for n = 5, the sample means can only take 6 distict values - 0 , 0.2, 0.4, 0.6, 0.8, 1. Due to the same reason the Q-Q plot shows horizontal bands.  As the sample size increases to 20 and 50, the distribution becomes more bell shaped and resembles normal distribution and the Q-Q plot show improved alignment with the reference line. 
#Bernoulli Distribution (p = 0.05):
  #The sample distribution remains right skewed for smaller sample sizes. Even at n = 50, it is right skewed. This is because the probability of success is small (p = 0.05), which results in relatively few successes with each sample.
#Overall the result show that the CLT describes the convergence of sampling distribution towards normal distribution as the value of n increases (n being sample size). However the rate of convergence depends on the population distribution. Skewed distributions and distributions with low probability of success may require comparitevly larger n value (sample size) for a reasonable normal appoximation.
