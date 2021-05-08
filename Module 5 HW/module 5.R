library(dplyr)

set.seed(11123)
instrumental = as.data.frame(rnorm(1000,mean = 1,sd = sqrt(2)))
colnames(instrumental) = "x_i"
instrumental = instrumental %>%
#  mutate(w_star = (1/sqrt(2*pi*1)*exp(-(x_i-0)^2/(2*1)))/(1/sqrt(2*pi*2)*exp(-(x_i-1)^2/(2*2)))) %>%
  mutate(w_star = dnorm(x_i,mean=0,sd = 1)/dnorm(x_i,mean=1,sd=sqrt(2)))%>%
  mutate(w_star_weighted = w_star/sum(w_star)) %>%
  mutate(h_x_weighted_standardized = x_i*w_star_weighted) %>%
  mutate(h_x_weighted_unstandardized = x_i*w_star)

sum(instrumental$h_x_weighted_standardized)

mean(instrumental$h_x_weighted_unstandardized)
var(instrumental$h_x_weighted_unstandardized)/length(instrumental$h_x_weighted_unstandardized)
hist(instrumental$h_x_weighted_unstandardized)
hist(instrumental$h_x_weighted_standardized)
hist(rnorm(1000,0,1))


##Problem 4
mixture.dat = read.table("/Users/Yichen/Documents/JHU/Computational Statistics/Data/mixture.dat",header=TRUE)
y = mixture.dat$y
n = 10000
test = NULL



f = function(x){prod(x*dnorm(y,7,0.5) + (1-x)*dnorm(y,10,0.5))}
R = function(x_t,x_star){f(x_star)*g(x_t,x_t)/(f(x_t)*g(x_star,x_t))}

g = function(x,x_t){dnorm(x,x_t,.01)}
test[1] = 15
for(i in 1:n){
  x_t = test[i]
  x_star = rnorm(1,x_t,.01)
  r = R(x_t,x_star)
  if(r>=1){
    test[i+1] = x_star
  } else{
    u = runif(1,0,1)
    if(u<r){
      test[i+1] = x_star
    } else{
      test[i+1] = x_t
    }
  }
}
plot(test[200:n+1], type = "l")
title("Sample path for N(x_t,.01^2) Proposal Dist.")
hist(test[200:n+1])

##Part 2
unif_values = NULL
n=10000
f = function(x){prod(x*dnorm(y,7,0.5) + (1-x)*dnorm(y,10,0.5))}
R_unif = function(x_t,x_star){f(x_star)*g_unif(x_t)/(f(x_t)*g_unif(x_star))}
g_unif = function(x){dunif(x,0,20)}
unif_values[1] = 7
for(i in 1:n){
  x_t = unif_values[i]
  x_star = runif(1,0,20)
  r = R_unif(x_t,x_star)
  if(r>=1){
    unif_values[i+1] = x_star
  } else{
    u = runif(1,0,1)
    if(u<r){
      unif_values[i+1] = x_star
    } else{
      unif_values[i+1] = x_t
    }
  }
}
plot(unif_values[200:n+1],type="l", ylim = c(6,22))
hist(unif_values[200:n+1])
