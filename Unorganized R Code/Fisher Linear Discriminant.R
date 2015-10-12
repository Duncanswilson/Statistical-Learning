
##Read in the Data 
X <- as.matrix(read.table(gzfile("~/zip.train.gz"))) ##read in training data 
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

##Seperate the 2's and 3's of training data  
TwosRows<- which(X[,1] == 2 ) 
ThreesRows <- which(X[,1] == 3)
Twos = X[TwosRows,-1]
Threes = X[ThreesRows,-1]

##Calculate the covarience of these two classes 
X.covarTwo =  cov(Twos)
X.covarThree = cov(Threes)

##Calculate column means of each 
X.twoMeans = colMeans(Twos)
X.threeMeans = colMeans(Threes)

##Sum the covariences of the two classes 
X.covarTotal = X.covarTwo + X.covarThree

##Take the inverse of the summed covariences (the scatters)
X.covarInverse = solve(X.covarTotal)
N2<- dim(Twos)[1]
N3 <- dim(Threes)[1]
X.covarInverse2 = solve((1/N2)*(t(Twos)%*%Twos)   +(1/N3)*(t(Threes)%*%Threes))

##Compute the best line to fit the data 
line = X.covarInverse2  %*% (X.twoMeans-X.threeMeans)

##Project the 256 dimensional data onto one dimension 
Twos1D = Twos %*% line
Threes1D= Threes %*% line

##Seperate the test data 
TwosandThrees.test <- (which(X.test[,1] == 2 | X.test[,1] == 3))
y.test <- X.test[TwosandThrees.test, 1]  
X.test= X.test[TwosandThrees.test, -1]
X.test1D = t(line) %*% t(X.test)
X.test1D
## Compute the standard deviation and means of twos and threes
Twos1D.sd = sd(Twos1D)
Threes1D.sd = sd(Threes1D)
Twos1D.mean = mean(Twos1D)
Threes1D.mean = mean(Threes1D)
length(X.test1D)
prediction = numeric(length(X.test1D))

for(i in 1:length(X.test1D))
{ 
  x = X.test1D[i]
  prob.two = 1/(Twos1D.sd*sqrt(2*pi))*exp((-(x-Twos1D.mean)^2)/2*Twos1D.sd^2)
  prob.three = 1/(Threes1D.sd*sqrt(2*pi))*exp((-(x-Threes1D.mean)^2)/2*Threes1D.sd^2)
  if(prob.two > prob.three){
  prediction[i] = 2
  }
  else{
    prediction[i] = 3 
  }
}

error = mean(prediction != y.test)
correct = 1-error

hist(Twos1D,xlim=c(-2,2),ylim=c(0,200),breaks=10,col=rgb(1,1,0,0.7),main="",xlab="One-Dimensional Projection")
par(new=TRUE)
hist(Threes1D,xlim=c(-2,2),ylim=c(0,200),breaks=10,col=rgb(0,1,1,0.4),main="",xlab="",ylab="")

legend("bottomright", c("Threes", "Twos"), fill=c(rgb(0,1,1,0.4), rgb(1,1,0,0.7)))



