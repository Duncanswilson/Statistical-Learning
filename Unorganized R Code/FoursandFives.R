
##Read in the Data 
X <- as.matrix(read.table(gzfile("~/zip.train.gz"))) ##read in training data 
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

##Seperate the 2's and 3's of training data  
FoursRows<- which(X[,1] == 4) 
FivesRows <- which(X[,1] == 5)
Fours = X[FoursRows,-1]
Fives = X[FivesRows,-1]

##Calculate the covarience of these two classes 
X.covarFour =  cov(Fours)
X.covarFive = cov(Fives)

##Calculate column means of each 
X.fourMeans = colMeans(Fours)
X.fiveMeans = colMeans(Fives)

##Sum the covariences of the two classes 
X.covarTotal = X.covarFour + X.covarFive

##Take the inverse of the summed covariences (the scatters)
X.covarInverse = solve(X.covarTotal)
N2<- dim(Fours)[1]
N3 <- dim(Fives)[1]
X.covarInverse2 = solve((1/N2)*(t(Fours)%*%Fours)   +(1/N3)*(t(Fives)%*%Fives))

##Compute the best line to fit the data 
line = X.covarInverse  %*% (X.fourMeans-X.fiveMeans)

##Project the 256 dimensional data onto one dimension 
Fours1D = Fours %*% line
Fives1D= Fives %*% line

##Seperate the test data 
FoursandFives.test <- (which(X.test[,1] == 4 | X.test[,1] == 5))
y.test<- X.test[FoursandFives.test, 1]  
X.test= X.test[FoursandFives.test, -1]
X.test1D = t(line) %*% t(X.test)
X.test1D
## Compute the standard deviation and means of Fours and Fives
Fours1D.sd = sd(Fours1D)
Fives1D.sd = sd(Fives1D)
Fours1D.mean = mean(Fours1D)
Fives1D.mean = mean(Fives1D)
length(X.test1D)

prediction = numeric(length(X.test1D))

for(i in 1:length(X.test1D))
{ 
  x = X.test1D[i]
  prob.four = 1/(Fours1D.sd*sqrt(2*pi))*exp((-(x-Fours1D.mean)^2)/2*Fours1D.sd^2)
  prob.five = 1/(Fives1D.sd*sqrt(2*pi))*exp((-(x-Fives1D.mean)^2)/2*Fives1D.sd^2)
  if(prob.four > prob.five){
    prediction[i] = 4
  }
  else{
    prediction[i] = 5 
  }
}

error = mean(prediction!= y.test)
correct = 1-error

hist(Fours1D,xlim=c(70,140),ylim=c(0,400),breaks=10,col=rgb(1,1,0,0.7),main="",xlab="One-Dimensional Projection")
par(new=TRUE)
hist(Fives1D,xlim=c(70,140),ylim=c(0,400),breaks=10,col=rgb(0,1,1,0.4),main="",xlab="",ylab="")

legend("topright", c("Fives", "Fours"), fill=c(rgb(0,1,1,0.4), rgb(1,1,0,0.7)))



