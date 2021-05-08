##Problem 2 part 2
rho = .1
itr = 10000
u_1 = 0
u_2 = 0
s_1 = 1
s_2 = 1
x_1 = NULL
x_2 = NULL
x_1[1] = 0
x_2[1] = 0

for(i in 1:itr){
  x_1[i+1] = rnorm(1,u_1+rho*s_1/s_2*(x_2[i]-u_2),sqrt(s_1^2*(1-rho^2)))
  x_2[i+1] = rnorm(1,u_2+rho*s_2/s_1*(x_1[i+1]-u_1),sqrt(s_2^2*(1-rho^2)))
}

plot(x_1,x_2)

mean(x_1)
var(x_1)
mean(x_2)
var(x_2)
cor(x_1,x_2)
