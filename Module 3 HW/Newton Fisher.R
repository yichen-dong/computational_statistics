library(dplyr)
library(matlib)
oilspills = read.table("oilspills.dat", header = TRUE)
oilspills = oilspills %>% 
  mutate(spills_import_rate = spills/importexport, spills_domestic_rate = spills/domestic)

##Initial values
##b1 is import/export, b2 is domestic, same for x[1] and x[2]
##Ni will be the spills
x=c(0.1,1/2)
itr = 40
x_values = matrix(0,itr+1,2)
x_values[1,] = x

g_prime = function(x){
  g_prime_da1 = with(oilspills, sum(importexport*spills/(x[1]*importexport + x[2]*domestic)) - sum(importexport))
  g_prime_da2 = with(oilspills, sum(domestic*spills/(x[1]*domestic + x[2]*domestic)) - sum(domestic))
  out = matrix(c(g_prime_da1,g_prime_da2),ncol = 1)
  return(out)
}
g_2prime = function(x){
  g_2prime_da1_2 = with(oilspills, -sum(importexport^2*spills/(x[1]*importexport + x[2]*domestic)^2))
  g_2prime_da2_2 = with(oilspills, -sum(domestic^2*spills/(x[1]*importexport + x[2]*domestic)^2))
  g_2prime_da1da2 = with(oilspills,-sum(importexport*domestic*spills/(x[1]*importexport + x[2]*domestic)^2))
  out = matrix(c(g_2prime_da1_2,g_2prime_da1da2,g_2prime_da1da2,g_2prime_da2_2), nrow = 2, byrow = TRUE)
  return(out)
} 

##main
##Converges only for initial values between 0 and 1. 
for(i in 1:itr){
  x= x - solve(g_2prime(x))%*%g_prime(x)
  x_values[i+1,] = x
}
##Final answer for Newton's: [0.6292309, 1.9711071]
x_values[41,]

##Start of Fisher
fisher = function(x){
  fisher_da1_2 = with(oilspills,-sum(importexport^2/(x[1]*importexport+x[2]*domestic)))
  fisher_da2_2 = with(oilspills,-sum(domestic^2/(x[1]*importexport+x[2]*domestic)))
  fisher_da1da2 = with(oilspills,-sum(importexport*domestic/(x[1]*importexport+x[2]*domestic)))
  out = -matrix(c(fisher_da1_2,fisher_da1da2,fisher_da1da2,fisher_da2_2),nrow = 2,byrow = TRUE)
  return(out)
}

fisher(x_1)
x_1 = c(.6,1.9)
itr = 40
x_values_1 = matrix(0,itr+1,2)
x_values_1[1,] = x_1
###fisher. Does not converge. I think the function might be wrong
for(i in 1:itr){
  x_1 = x_1-solve(fisher(x_1))%*%g_prime(x_1)
  x_values_1[i+1,] = x_1
}

##Ascent algorithm (steepest)
x_2 = c(.1,1)
itr = 40
M_t = matrix(c(1,0,0,1),nrow = 2,byrow = TRUE)
x_values_2 = matrix(0,itr+1,2)
x_values_2[1,] = x_2
for(i in 1:itr){
  x_2 = x_2-1/16*solve(-M_t)%*%g_prime(x_2)
  x_values_2[i+1,]=x_2
}

##Ascent final answer: [0.6292562, 1.9710526]
x_values_2[41,]

##I think that the Newton was decently easy to implement and decently easy to calculate. The Fisher was probably the hardest to calculate
##(I'm not even sure I got it right), but easy to implement once the Newton was set up. The Ascent algorithm was easy to calculate as well,
##but selecting the right alpha adds an extra level of complexity to it. 