X.train <- as.matrix(read.table(gzfile("~/zip.train.gz")))
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

##Seperate the 2's and 3's of training data  
TwosandThrees <- (which(X.train[,1] == 2 | X.train[,1] == 3))
Y.train <- as.matrix(X.train[TwosandThrees, 1] - 2 )  
X.subset = X.train[TwosandThrees,]

##Set the first col to zero 
X.subset[,1] = 1     

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
TwosandThrees.test <- (which(X.test[,1] == 2 | X.test[,1] == 3))
Y.test <- (X.test[TwosandThrees.test, 1] - 2 )  
X.testSubset = X.test[TwosandThrees.test,]
X.testSubset[,1] =1

X.prediction = numeric(nrow(X.testSubset))
threeCorrect = 0
twoCorrect = 0 
## test the predictive ability of your parameters 
for(i in 1:nrow(X.testSubset)){
if( exp(t(beta)%*%X.testSubset[i,])/(1+exp(t(beta)%*%X.testSubset[i,])) > 1/(1+exp(t(beta)%*%X.testSubset[i,]))){
  X.prediction[i] = 1
  if(Y.test[i] == 1){ 
   threeCorrect = threeCorrect + 1  
  }
 }
 else{
   X.prediction[i] = 0 
   if(Y.test[i] == 0){
     twoCorrect = twoCorrect + 1
   }
 }
}

## set up variables to load in the built int pred
X.BIprediction = numeric(nrow(X.testSubset))
BIthreeCorrect = 0
BItwoCorrect = 0 

## test the predictive ability of the built-in functions parameters 
for(i in 1:nrow(X.testSubset)){
  if( exp(t(beta2)%*%X.testSubset[i,])/(1+exp(t(beta2)%*%X.testSubset[i,])) > 1/(1+exp(t(beta2)%*%X.testSubset[i,]))){
    X.BIprediction[i] = 1
    if(Y.test[i] == 1){ 
      BIthreeCorrect = BIthreeCorrect + 1  
    }
  }
  else{
    X.BIprediction[i] = 0 
    if(Y.test[i] == 0){
      BItwoCorrect = BItwoCorrect + 1
    }
  }
}

##create table of prediction data 
a<- data.frame(table(Y.test))
a <- cbind(a,numberCorrect=1)
a <- cbind(a,builtInCorrect=1)
a[2,3] = threeCorrect
a[1,3] = twoCorrect
a[2,4] = BIthreeCorrect
a[1,4] = BItwoCorrect
a[,1] = t(cbind(2,3))
a.table <- xtable(a, "2's vs. 3's")
a.table

