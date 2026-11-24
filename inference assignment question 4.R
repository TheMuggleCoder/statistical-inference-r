#Question 4
#Question 4.1
#Bootstrapping: For a dataset containing n observations :
# Step 1 : Make a bootstrapped dataset by randomly selection n observations, with replacement from the original dataset to create a new sample.
# Step 2 : Calculate Statistic of Interest (eg : mean or median)
# Step 3 : Keep track of that calculation
# Step 4 : Repeat steps 1 to 3 a bunch of times.
# Step 5 : Use the newly formed distribution to calculate statistics to estimate a confidence interval.

#Question 4.2
set.seed(48703186)

x <-  rexp(100, rate = 1)
b_medians <-  numeric(1000)
for( i in 1:1000) {
  b_sample <- sample(x, size = 100, replace = TRUE)
  b_medians[i] <-  median(b_sample)
}
CI <-  quantile(b_medians, probs = c(0.025, 0.975))
CI

install.packages('ggplot2')
library(ggplot2)
ggplot(data.frame(median=b_medians), aes(x=median)) +
  geom_histogram(bins=40, fill="#DDA15E", colour="#264653") +
  geom_vline(xintercept=CI, colour="#E76F51", linetype="dashed") +
  labs(title="Bootstrap Distribution of Sample Medians",
       x="Bootstrap Median",
       y="Frequency")

# The graph shows the variability of sample median (repeated sampling with replacement). The plot is slightly right skewed and the 95% CI is approx (0.395, 0.818)

#Question 4.3
#after calculation the true population median = 0.6931
set.seed(48703186)
n <- 100
lambda <- 1
true_median <- log(2)
cover <- logical(1000)
for(i in 1:1000) {
  x <- rexp(100, rate=1)
  b_medians <- numeric(1000)
  for(j in 1:1000) {
    b_sample <- sample(x, size=100, replace=TRUE)
    b_medians[j] <- median(b_sample)
  }
  CI <- quantile(b_medians,
                 probs=c(0.025, 0.975))
  cover[i] <- (CI[1] <= true_median &&
                    true_median <= CI[2])
}
cover_prob <- mean(cover)
cover_prob

#the bootstrap CI coverage is 94.3%, which is very close to the expected 95%.
#This means that 943 out of 1000 CI contained the true population median of 0.6931. the small difference from 95% can be attributed to random variation in the simulation.

#Question 4.4
set.seed(48703186)
n <- 100
lambda <- 1
true_median <- log(2)
lambda_cover <- logical(1000)
median_cover <- logical(1000)
for(i in 1:1000) {
  x <- rexp(n, rate=lambda)
  lambda_hat <- 1/mean(x)
  se_lambda <- lambda_hat/sqrt(n)
  lambda_lower <- lambda_hat - 1.96*se_lambda
  lambda_upper <- lambda_hat + 1.96*se_lambda
  median_hat <- log(2)/lambda_hat
  se_median <- median_hat/sqrt(n)
  median_lower <- median_hat - 1.96*se_median
  median_upper <- median_hat + 1.96*se_median
  lambda_cover[i] <- (lambda_lower <= lambda &&
                        lambda <= lambda_upper)
  median_cover[i] <- (median_lower <= true_median &&
                        true_median <= median_upper)
}
lambda_coverage <- mean(lambda_cover)
median_coverage <- mean(median_cover)

lambda_coverage
median_coverage

#The estimated coverage for the 95% wald interval for lambda is 95.3%, while for the median it is 94.1%. Both values are close to the expected 95% coverage level. This shows that the wald CI perform well for both the parameters. The slight difference can be related to random variations across the 1000 simulation samples.


#Question 4.5
set.seed(48703186)
lambda <- 1
true_median <- log(2)
n_values <- c(10, 50)
results <- data.frame()
for(n in n_values) {
  bootstrap_cover <- logical(1000)
  lambda_cover <- logical(1000)
  median_cover <- logical(1000)
  for(i in 1:1000) {
    x <- rexp(n, rate=lambda)
    b_medians <- numeric(1000)
    for(j in 1:1000) {
      b_sample <- sample(x, size=n, replace=TRUE)
      b_medians[j] <- median(b_sample)
    }
    boot_CI <- quantile(b_medians, probs=c(0.025, 0.975))
    
#the result shows that the performance of CIs generally improve with increasing sample size. For the bootstrap confidence interval for the median, coverage increases from 92.8% at n = 10 to 94.2% at n = 100.
#The Wald confidence interval for lambda maintains coverage close to the nominal 95% level across all three sample sizes. 
#However the Wald CI for the median performs less accurately for the smaller sample size, with coverage of 91.9% at n - 10, improving to 94.7% at n = 50.
#Overall the result shows that the sample sizes improve the reliability of the CI. the normal approximation used in the Wald method is less accurate for the smaller samples, especially when estimating the meadian. Small difference from 95% are also expected due to random variation across 1000 simulations. 
    
    bootstrap_cover[i] <- (boot_CI[1] <= true_median &&
                             true_median <= boot_CI[2])
    lambda_hat <- 1/mean(x)
    se_lambda <- lambda_hat/sqrt(n)
    lambda_lower <- lambda_hat - 1.96*se_lambda
    lambda_upper <- lambda_hat + 1.96*se_lambda
    lambda_cover[i] <- (lambda_lower <= lambda &&
                          lambda <= lambda_upper)
    median_hat <- log(2)/lambda_hat
    se_median <- median_hat/sqrt(n)
    median_lower <- median_hat - 1.96*se_median
    median_upper <- median_hat + 1.96*se_median
    median_cover[i] <- (median_lower <= true_median &&
                          true_median <= median_upper)
  }
  results <- rbind(results, data.frame(
    Sample_Size=n,
    Bootstrap_Median=mean(bootstrap_cover),
    Wald_Lambda=mean(lambda_cover),
    Wald_Median=mean(median_cover)
  ))
}
results