X.train <- as.matrix(read.table(gzfile("~/zip.train.gz")))
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

##Seperate the 4's and 5's of training data  
FoursandFives <- (which(X.train[,1] == 4 | X.train[,1] == 5))
Y.train <- as.matrix(X.train[FoursandFives, 1] - 4 )  
X.subset = X.train[FoursandFives,]

##Set the first col to 1 
X.subset[,1] = 1     
dim(Y.train)
dim(X.subset)
##make beta vector 
beta <- rnorm(ncol(X.subset), mean = 0, sd = .5)

##define the likelihood function 
likelihood <-function(beta, y, x){
  return(-sum(y*(x %*%beta - log(1+exp(x %*% beta))) + (1-y)*(-log(1 + exp(x %*% beta))))) 
}

##find the coefficents with optim function 
logit <- optim(beta, likelihood, x = X.subset, y = Y.train, method = 'BFGS', hessian=TRUE)

##transfer optimized parameters 
beta <-logit$par

##find the coefficents with the built in  function 
logit2 <- glm(Y.train ~ X.subset,family = binomial(link= "logit"))
beta2 <- logit2$coefficients[-2]

##seperate test data 
FoursandFives.test <- (which(X.test[,1] == 4 | X.test[,1] == 5))
Y.test <- (X.test[FoursandFives.test, 1] - 4 )  
X.testSubset = X.test[FoursandFives.test,]
X.testSubset[,1] =1

##prep data to be loaded with prediction data
X.prediction = numeric(nrow(X.testSubset))
FiveCorrect = 0
FourCorrect = 0 

## test the predictive ability of your parameters 
for(i in 1:nrow(X.testSubset)){
  if( exp(t(beta)%*%X.testSubset[i,])/(1+exp(t(beta)%*%X.testSubset[i,])) > 1/(1+exp(t(beta)%*%X.testSubset[i,]))){
    X.prediction[i] = 1
    if(Y.test[i] == 1){ 
      FiveCorrect = FiveCorrect + 1  
    }
  }
  else{
    X.prediction[i] = 0 
    if(Y.test[i] == 0){
      FourCorrect = FourCorrect + 1
    }
  }
}

## set up variables to load in the built int pred
X.BIprediction = numeric(nrow(X.testSubset))
BIFiveCorrect = 0
BIFourCorrect = 0 
## test the predictive ability of the built-in functions parameters 
for(i in 1:nrow(X.testSubset)){
  if( exp(t(beta2)%*%X.testSubset[i,])/(1+exp(t(beta2)%*%X.testSubset[i,])) > 1/(1+exp(t(beta2)%*%X.testSubset[i,]))){
    X.BIprediction[i] = 1
    if(Y.test[i] == 1){ 
      BIFiveCorrect = BIFiveCorrect + 1  
    }
  }
  else{
    X.BIprediction[i] = 0 
    if(Y.test[i] == 0){
      BIFourCorrect = BIFourCorrect + 1
    }
  }
}

##load prediction data in to a table 
a<- data.frame(table(Y.test))
a <- cbind(a,numberCorrect=1)
a <- cbind(a,builtInCorrect=1)
a[2,3] = FiveCorrect
a[1,3] = FourCorrect
a[2,4] = BIFiveCorrect
a[1,4] = BIFourCorrect
a[,1] = t(cbind(2,3))
a.table <- xtable(a, "4's vs. 5's")
a.table
