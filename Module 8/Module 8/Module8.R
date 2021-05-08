## Problem 1
library(dplyr)
CV_X = scan("CV_X.txt")
CV_Y = scan("CV_Y.txt")
mean_CV = mean(CV_X)
CV = as.data.frame(cbind(CV_X,CV_Y))
CV = CV %>%
  mutate(normal = dnorm(CV_X,mean_CV,sqrt(2)))%>%
  mutate(error = abs(CV_Y-normal))
plot(CV_X,CV_Y)
points(CV_X,CV$normal,col = "red")
sum(CV$error)/length(CV$CV_X)
### Part b
CV = CV %>%
  mutate(group = floor((as.numeric(rownames(CV))-1)/50))
mean_CV0 = mean(CV$CV_X[CV$group == 0])
mean_CV1 = mean(CV$CV_X[CV$group == 1])
CV = CV %>%
  mutate(norm_group_0 = dnorm(CV_X,mean_CV1,sqrt(2))
         ,norm_group_1 = dnorm(CV_X,mean_CV0,sqrt(2)))
CV = CV%>%
  mutate(error_0 = ifelse(group == 0, abs(CV_Y - norm_group_0),0)
         ,error_1 = ifelse(group == 1, abs(CV_Y - norm_group_1),0))
(sum(CV$error_0)+sum(CV$error_1))/length(CV$CV_X)

## Problem 2
jackknife = scan("Jackknife.txt")
jackknife = as.data.frame(jackknife)
jack_mean = mean(jackknife$jackknife)
b2 = sum((jackknife$jackknife -jack_mean)^4)/sum((jackknife$jackknife -jack_mean)^2)^2

k=5
groups = length(jackknife$jackknife)/k
jack_group = rep(1:groups,each =k)
jackknife= cbind.data.frame(jackknife,jack_group)
T_minus_j = NULL

for(i in 1:groups){
  jk_minus_j = jackknife$jackknife[jackknife$jack_group != i]
  jk_minus_j_mean = mean(jk_minus_j)
  T_minus_j[i] = sum((jk_minus_j -jk_minus_j_mean)^4)/sum((jk_minus_j -jk_minus_j_mean)^2)^2
}

T_bar_dot = mean(T_minus_j)
J_T = groups*b2 - (groups-1)*T_bar_dot

jk_var = (groups-1)/groups * sum((T_minus_j - T_bar_dot)^2)

### Part c
