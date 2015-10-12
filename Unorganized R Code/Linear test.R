data <- read.csv("~/data.csv")
y = data$profit
x = data$population 
x
x = cbind(1,x)
theta = c(0,0,0,0,0,0,0,0)
m = nrow(x)
m
cost <- sum(((x%*%theta)- y)^2)/(2*m)
cost
alpha <- 0.001
iterations <- 1500
for(i in 1:iterations)
{
  theta[1] <- theta[1] - alpha * (1/m) * sum(((x%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/m) * sum(((x%*%theta)- y)*x[,2])
}
