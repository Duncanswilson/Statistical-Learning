##Read in the Data 
X <- as.matrix(read.table(gzfile("~/zip.train.gz"))) ##read in training data 
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

##Seperate the 2's and 3's of training data  
SevensRows<- which(X[,1] == 7) 
NinesRows <- which(X[,1] == 9)
Sevens = X[SevensRows,-1]
Nines = X[NinesRows,-1]

##Calculate the covarience of these two classes 
X.covarSeven =  cov(Sevens)
X.covarNine = cov(Nines)

##Calculate column means of each 
X.sevenMeans = colMeans(Sevens)
X.nineMeans = colMeans(Nines)

##Sum the covariences of the two classes 
X.covarTotal= X.covarSeven + X.covarNine

##Take the inverse of the summed covariences (the scatters)
X.covarInverse = solve(X.covarTotal + .00001*diag(256))
N2<- dim(Sevens)[1]
N3 <- dim(Nines)[1]
X.covarInverse2 = solve((1/N2)*(t(Sevens)%*%Sevens) +(1/N3)*(t(Nines)%*%Nines))

##Compute the best line to fit the data 
line = X.covarInverse  %*% (X.sevenMeans-X.nineMeans)

##Project the 256 dimensional data onto one dimension 
Sevens1D = Sevens %*% line
Nines1D= Nines %*% line

##Seperate the test data 
SevensandNines.test <- (which(X.test[,1] == 7 | X.test[,1] == 9))
y.test<- X.test[SevensandNines.test, 1]  
X.test= X.test[SevensandNines.test, -1]
X.test1D = t(line) %*% t(X.test)
X.test1D
## Compute the standard deviation and means of Sevens and Nines
Sevens1D.sd = sd(Sevens1D)
Nines1D.sd = sd(Nines1D)
Sevens1D.mean = mean(Sevens1D)
Nines1D.mean = mean(Nines1D)
length(X.test1D)

prediction = numeric(length(X.test1D))

for(i in 1:length(X.test1D))
{ 
  x = X.test1D[i]
  prob.seven = 1/(Sevens1D.sd*sqrt(2*pi))*exp((-(x-Sevens1D.mean)^2)/2*Sevens1D.sd^2)
  prob.nine = 1/(Nines1D.sd*sqrt(2*pi))*exp((-(x-Nines1D.mean)^2)/2*Nines1D.sd^2)
  if(prob.seven > prob.nine){
    prediction[i] = 7
  }
  else{
    prediction[i] = 9
  }
}

error = mean(prediction!= y.test)
correct = 1-error

hist(Sevens1D,xlim=c(-100,-60),ylim=c(0,200),breaks=10,col=rgb(1,1,0,0.7),main="",xlab="One-Dimensional Projection")
par(new=TRUE)
hist(Nines1D,xlim=c(-100,-60),ylim=c(0,200),breaks=10,col=rgb(0,1,1,0.4),main="",xlab="",ylab="")

legend("topright", c("Nines", "Sevens"), fill=c(rgb(0,1,1,0.4), rgb(1,1,0,0.7)))



