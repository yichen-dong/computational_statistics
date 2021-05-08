## Problem 1 (9.5)
library(dplyr)
stomach = c(25,42,45,46,51,103,124,146,340,396,412,876,1112)
breast = c(24,40,719,727,791,1166,1235,1581,1804,3460,3808)
log_stomach = log(stomach)
log_breast = log(breast)
mean_log_stomach = mean(log_stomach)
mean_log_breast = mean(log_breast)

b = 10000
bootstrap_theta_st = NULL
for(i in 1:b){
  n = length(stomach)
  data_bootstrap = sample(log_stomach,size = n, replace = TRUE)
  bootstrap_mean = mean(data_bootstrap)
  bootstrap_theta_st[i] = bootstrap_mean
}
bootstrap_theta_bar_st = mean(bootstrap_theta_st)
bootstrap_variance_st = 1/(b-1)*sum((bootstrap_theta_st-bootstrap_theta_bar_st)^2)
hist(bootstrap_theta_st)
#Finding the CI using an alpha of .05 and using the Percentile Method
sorted_bootstrap_theta_st = sort(bootstrap_theta_st)
bootstrap_theta_CI_st = c(bootstrap_theta_bar_st- sqrt(bootstrap_variance_st)*qnorm(.975),bootstrap_theta_bar_st +sqrt(bootstrap_variance_st)*qnorm(.975))
bootstrap_theta_CI_st_pct = c(sorted_bootstrap_theta_st[round(b*.025)],sorted_bootstrap_theta_st[round(b*.975)])

#For breasts
b = 10000
bootstrap_theta_br = NULL
for(i in 1:b){
  n = length(breast)
  data_bootstrap = sample(log_breast,size = n, replace = TRUE)
  bootstrap_mean = mean(data_bootstrap)
  bootstrap_theta_br[i] = bootstrap_mean
}
bootstrap_theta_bar_br = mean(bootstrap_theta_br)
bootstrap_variance_br = 1/(b-1)*sum((bootstrap_theta_br-bootstrap_theta_bar_br)^2)
hist(bootstrap_theta_br)
#Finding the CI using an alpha of .05 and using the Percentile Method
sorted_bootstrap_theta_br = sort(bootstrap_theta_br)
bootstrap_theta_CI_br = c(bootstrap_theta_bar_br- sqrt(bootstrap_variance_br)*qnorm(.975),bootstrap_theta_bar_br +sqrt(bootstrap_variance_br)*qnorm(.975))
bootstrap_theta_CI_br_pct = c(sorted_bootstrap_theta_br[round(b*.025)],sorted_bootstrap_theta_br[round(b*.975)])

### Part b
# I'm not sure how to permute, so I'm just going to mix up the variables many many times and hope that's a good approximation
combined = c(log_stomach,log_breast)
itr = 100000
test_statistic = NULL
for(i in 1:itr){
  sample = sample(combined)
  permute_stomach = sample[1:13]
  permute_breast = sample[14:24]
  test_statistic[i] = mean(permute_stomach) - mean(permute_breast)
}
hist(test_statistic)
mean(test_statistic)
boxplot(test_statistic)

### Part C
exp_breast_ci = exp(bootstrap_theta_CI_br_pct)
b = 10000
bootstrap_theta_orig = NULL
for(i in 1:b){
  n = length(breast)
  data_bootstrap = sample(breast,size = n, replace = TRUE)
  bootstrap_mean = mean(data_bootstrap)
  bootstrap_theta_orig[i] = bootstrap_mean
}
bootstrap_theta_bar_orig = mean(bootstrap_theta_orig)
bootstrap_variance_orig = 1/(b-1)*sum((bootstrap_theta_orig-bootstrap_theta_bar_orig)^2)
hist(bootstrap_theta_orig)
#Finding the CI using an alpha of .05 and using the Percentile Method
sorted_bootstrap_theta_orig = sort(bootstrap_theta_orig)
bootstrap_theta_CI_orig = c(bootstrap_theta_bar_orig- sqrt(bootstrap_variance_orig)*qnorm(.975),bootstrap_theta_bar_orig +sqrt(bootstrap_variance_orig)*qnorm(.975))
bootstrap_theta_CI_orig_pct = c(sorted_bootstrap_theta_orig[round(b*.025)],sorted_bootstrap_theta_orig[round(b*.975)])

## Problem 2
cauchy = rcauchy(100)
mean(cauchy)
b = 10000
cauchy_bootstrap_means = NULL
for(i in 1:b){
  n = length(cauchy)
  bootstrap_cauchy = sample(cauchy,size = n,replace = TRUE)
  cauchy_bootstrap_means[i] = mean(bootstrap_cauchy)
}
hist(cauchy_bootstrap_means)
mean(cauchy_bootstrap_means)

#set seed
cauchy = rcauchy(100)
mean(cauchy)
b = 100
cauchy_bootstrap_means = NULL
for(i in 1:b){
  n = length(cauchy)
  bootstrap_cauchy = sample(cauchy,size = n,replace = TRUE)
  cauchy_bootstrap_means[i] = mean(bootstrap_cauchy)
}
hist(cauchy_bootstrap_means)
mean(cauchy_bootstrap_means)

# for estimating max
uniform = runif(10000,0,2)
max(uniform)
b = 10000
uniform_boostrap_max = NULL
for(i in 1:b){
  n = length(uniform)
  bootstrap_uniform = sample(uniform,size=n,replace= TRUE)
  uniform_boostrap_max[i] = max(bootstrap_uniform)
}
hist(bootstrap_uniform)
max(bootstrap_uniform)

uniform = runif(10000,0,2)
max(uniform)
b = 100
uniform_boostrap_max = NULL
for(i in 1:b){
  n = length(uniform)
  bootstrap_uniform = sample(uniform,size=n,replace= TRUE)
  uniform_boostrap_max[i] = max(bootstrap_uniform)
}
hist(bootstrap_uniform)
max(bootstrap_uniform)

#Problem 4
p4_rnorm = rnorm(100,0,1)
mean(p4_rnorm)
b = 10
p4_boot_mean = NULL
for(i in 1:b){
  n = length(p4_rnorm)
  bootstrap_norm = sample(p4_rnorm,size=n,replace=TRUE)
  p4_boot_mean[i] = mean(bootstrap_norm)
}
p4_theta_bar_star = mean(p4_boot_mean)
bias_corrected = 2*mean(p4_rnorm) - mean(p4_boot_mean)
p4_b_est_variance = 1/(b-1)*sum((p4_boot_mean - p4_theta_bar_star)^2)

##balanced bootstrap
b=10
balance_p4_rnorm = NULL
for(i in 1:b){
  balance_p4_rnorm = c(balance_p4_rnorm,p4_rnorm)
}
balance_p4_rnorm_permute = sample(balance_p4_rnorm, length(balance_p4_rnorm))
balance_p4_means = NULL
for(i in 1:b){
  start = i*100 -99
  end = i*100
  balance_p4_means[i] = mean(balance_p4_rnorm_permute[start:end])
}
p4_balance_theta_bar_star = mean(balance_p4_means)
balance_bias_corrected = 2*mean(p4_rnorm) - mean(balance_p4_means)
p4_balance_variance = 1/(b-1)*sum((balance_p4_means-p4_balance_theta_bar_star)^2)
