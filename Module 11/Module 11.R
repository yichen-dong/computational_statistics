## Part A
library(dplyr)
F12 = scan('F12.txt')
F12 = as.data.frame(F12)
F12 = F12 %>%
  mutate(X = log(F12))
n = length(F12$X)
sd = sd(F12$X)
Silverman_bw = (4/(3*n))^(1/5)*sd
Silverman_density = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    Silverman_density[i] = max(Silverman_density[i],0) + 1/(n*Silverman_bw)*(exp(-(F12$X[i] - F12$X[j])^2/(2*Silverman_bw^2))/sqrt(2*pi))
  }
}
F12 = F12 %>%
  cbind(Silverman_density)

F12 = F12%>%
  arrange(X)


hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$Silverman_density)

h_0 = Silverman_bw

R_f_d2 = 0
for (j in 1:length(F12$X)){
  for (i in 1:length(F12$X)){
    R_f_d2= -exp(-(F12$X[j] - F12$X[i])/h_0)/(32*n^2*h_0^9*pi)
  }
}
R_k = 1/(2*sqrt(pi))
sd = sd(F12$X)
SJ_bw = (R_k/(-n*sd^4*R_f_d2))^(1/5)
R_k/(h_0^5*n*sd^4)

#I couldn't figure out how to get R(f''(x)), so I used the method they had in the book code
SJ_bw = bw.SJ(F12$X)
SJ_density = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    SJ_density[i] = max(SJ_density[i],0) + 1/(n*SJ_bw)*(exp(-(F12$X[i] - F12$X[j])^2/(2*SJ_bw^2))/sqrt(2*pi))
  }
}
F12 = F12 %>%
  cbind(SJ_density)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$SJ_density)

Terrell_bw = 3*(R_k/(35*n))^(1/5)*sd
Terrell_density = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    Terrell_density[i] = Terrell_density[i] + 1/(n*Terrell_bw)*(exp(-(F12$X[i] - F12$X[j])^2/(2*Terrell_bw^2))/sqrt(2*pi))
  }
}
F12 = F12 %>%
  cbind(Terrell_density)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$Terrell_density)

##Part B
###Uniform
SJ_density_unif = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    SJ_density_unif[i] = SJ_density_unif[i] + 1/(n*SJ_bw)*(1/2)*(ifelse(abs((F12$X[i] - F12$X[j])/SJ_bw)<1,1,0))
  }
}
F12 = F12 %>%
  cbind(SJ_density_unif)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$SJ_density_unif)

###Epanechnikov
SJ_density_Ep = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    SJ_density_Ep[i] = SJ_density_Ep[i] + 1/(n*SJ_bw)*(3/4*(1-((F12$X[i]-F12$X[j])/SJ_bw)^2))*(ifelse(abs((F12$X[i] - F12$X[j])/SJ_bw)<1,1,0))
  }
}
F12 = F12 %>%
  cbind(SJ_density_Ep)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$SJ_density_Ep)

###Tri-weight
SJ_density_Tri = vector(mode = 'double',length = n)
for(i in 1:length(F12$X)){
  for(j in 1:length(F12$X)){
    SJ_density_Tri[i] = SJ_density_Tri[i] + 1/(n*SJ_bw)*(35/32*(1-((F12$X[i]-F12$X[j])/SJ_bw)^2)^3)*(ifelse(abs((F12$X[i] - F12$X[j])/SJ_bw)<1,1,0))
  }
}
F12 = F12 %>%
  cbind(SJ_density_Tri)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$SJ_density_Tri)

###Histogram
h_bins = seq(from = min(F12$X),to = max(F12$X),by=(max(F12$X)-min(F12$X))/20)
v_k = h_bins[2] - h_bins[1]
n_k = NULL
hist_estimator = vector(mode = 'double',length = n)
for (i in 1:(length(h_bins)-1)){
  n_k[i] = sum(ifelse((F12$X >= h_bins[i] & F12$X <= h_bins[i+1]),1,0))
}

for(i in 1:length(F12$X)){
  for(j in 1:length(n_k)){
    hist_estimator[i] = hist_estimator[i] + n_k[j]/(n*v_k)*ifelse((F12$X[i] >= h_bins[j] & F12$X[i] <= h_bins[j+1]),1,0)
  }
}
F12 = F12 %>%
  cbind(hist_estimator)
hist(F12$X,freq = FALSE,ylim=c(0,1),breaks = 20)
lines(F12$X,F12$hist_estimator,col='red')
