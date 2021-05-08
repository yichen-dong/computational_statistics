library(dplyr)
x = scan('smooth_x.txt')
y = scan('smooth_y.txt')
smooth = as.data.frame(cbind(x,y))%>%
  arrange(x)
plot(smooth$x,smooth$y)



n = length(smooth$x)
k_end = 11
S_k = NULL

CVRSS_p1 = vector("numeric",length = k_end)

for(k in 1:11){
  S = matrix(data = 0, nrow = n,ncol = n)
  span = 2*k+1
  b = (span-1)/2
  for(i in 1:n){
    
    if(i-b <= 0){
      left = i-1
    } else{
      left = b
    }
    
    if(i+b>n){
      right = 100-i
    } else{
      right = b
    }
    truncated_k = left+right+1
    S[i,(i-left):(i+right)] = 1/truncated_k
    S_k[i] = sum(smooth$y * S[i,])
  }
  smooth[paste("k_",k, sep = '')] = S_k
  for(i in 1:n){
    CVRSS_p1[k] = CVRSS_p1[k] + 1/n *((smooth$y[i]-S_k[i])/(1-S[i,i]))^2
  }
}
plot(CVRSS_p1)
plot(smooth$x,smooth$y)
lines(smooth$x,smooth$k_2,type = 'l')
lines(smooth$x,smooth$k_4,type = 'l',col='red')
lines(smooth$x,smooth$k_11,type = 'l',col='blue')

## Problem 2

smooth_p2 = as.data.frame(cbind(x,y))%>%
  arrange(x)
n = length(smooth_p2$x)
k_end = 11
S_k_med = NULL

for(k in 1:k_end){
  span = 2*k+1
  b = (span-1)/2
  for(i in 1:n){
    
    if(i-b <= 0){
      left = i-1
    } else{
      left = b
    }
    
    if(i+b>n){
      right = 100-i
    } else{
      right = b
    }
    cs_med = smooth_p2$y[(i-left):(i+right)]
    cs_med = sort(cs_med)
    if ((span-1)/2%%1 == 0){#Since R does not have a function to check for an integer, this checks to see if a decimal is returned
      S_k_med[i] = cs_med[(span-1)/2]
    }else{
      S_k_med[i] = (cs_med[floor((span-1)/2)] + cs_med[ceiling((span-1)/2)])/2
    }
  }
  smooth_p2[paste("k_",k, sep = '')] = S_k_med
}
plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_1)

plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_2)

plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_4)

plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_6)

plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_8)

plot(smooth_p2$x,smooth_p2$y)
lines(smooth_p2$x,smooth_p2$k_11)

## Problem 3
smooth_p3 = as.data.frame(cbind(x,y))%>%
  arrange(x)
h_array = seq(.1,1.1, by =.1)
n = length(smooth_p3$x)

K_z = function(z){
  1/sqrt(2*pi)*exp(-z^2/2)
}

for(h in h_array){
  S = matrix(data = 0, nrow = n,ncol = n)
  s_k_norm = NULL
  for(i in 1:n){
    for(j in 1:n){
      S[i,j] = K_z((smooth_p3$x[i] - smooth_p3$x[j])/h)
    }
    sum_i = sum(S[i,])
    s_k_norm[i] = 1/sum_i * sum(smooth_p3$y * S[i,])
  }
  smooth_p3[paste("h_",h, sep = '')] = s_k_norm
  plot(smooth_p3$x,smooth_p3$y)
  lines(smooth_p3$x, s_k_norm)
  title(main = paste("Graph for h=" , h))
}

for(h in c(.1)){
  S = matrix(data = 0, nrow = n,ncol = n)
  s_k_norm = NULL
  for(i in 1:n){
    for(j in 1:n){
      S[i,j] = K_z((smooth_p3$x[i] - smooth_p3$x[j])/h)
    }
    sum_i = sum(S[i,])
    s_k_norm[i] = 1/sum_i * sum(smooth_p3$y * S[i,])
  }
}
