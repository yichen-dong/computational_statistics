library(dplyr)
library(ggplot2)
set.seed(121)
x = runif(5000,0,1)
montycarlo = data.frame(x)
montycarlo = montycarlo %>% 
  mutate(g_x = 4*sqrt(1-x^2))

I_hat = with(montycarlo, 1/length(montycarlo$x)*sum(g_x))

## Problem 2
x_1 = runif(1000,0,1)
inverse_cdf = data.frame(x_1)
inverse_cdf = inverse_cdf %>%
  mutate(xval = ifelse(x_1<=.5, sqrt(x_1/2),-(-4+sqrt(-8*x_1+8))/4))
hist(inverse_cdf$xval)
## Problem 2 rejection sampling
## Suppose we take a g(x) to be a uniform distribution at y=1 from 0 to 1.
## We can easily see that the cdf from 0 to 1 is 1. In order for this to encompass
## our f(x), we need to divide by alpha = .5, so that y=2, which is that max of our 
## original f(x). 

x_star = c()
seed = 1
while(length(x_star)<1000){
  set.seed(seed)
  x_s= runif(1,0,1)
  u = runif(1,0,1)
  f_x_s = ifelse(x_s<=.5,4*x_s,4-4*x_s)
  if(u<f_x_s/2){
    x_star = c(x_star,x_s)
  }
  seed = seed + 1
}
hist(x_star)

## Calculating E(X^2)
sum(inverse_cdf$xval^2)/length(inverse_cdf$xval)

## Problem 3
x_2 <- seq(-5, 7, length=1000)
y <- dnorm(x_2, mean=0, sd=1)
z = dnorm(x_2,mean=1,sd=sqrt(25))/.15
#y = 1/sqrt(2*pi*1)*exp(-(x_2-0)^2/(2*1))
#z = 1/sqrt(2*pi*2)*exp(-(x_2-1)^2/(2*2))/.3
plot(x_2, y, type="l", lwd=1,ylim = c(0,.8))
lines(x_2,z)

seed = 50
x_norm = c()
while(length(x_norm) < 10000){
  set.seed(seed)
  x_n = rnorm(1,1,sqrt(2))
  u = runif(1,0,1)
 # f_x_n = 1/sqrt(2*pi*1)*exp(-(x_n-0)^2/(2*1))
 # g_x_n = 1/sqrt(2*pi*2)*exp(-(x_n-1)^2/(2*2))/.3
  f_x_n = dnorm(x_n,mean=0,sd=1)
  g_x_n = dnorm(x_n,mean=1,sd=sqrt(2))/.3
  if(u<=f_x_n/g_x_n){
    x_norm = c(x_norm,x_n)
  }
  seed = seed + 1
}
hist(x_norm)
mean(x_norm)
var(x_norm)
