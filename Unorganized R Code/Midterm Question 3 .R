## load data through the elemStatLearn R package 
## explore the data 
pairs(SAheart)

## seperate train and test data and targets 
train = (SAheart[1:309,])
test = (SAheart[310:462,-10])
test.target = (SAheart[310:462, 10])

## cast both the data as a numerical matrix  
trainN = sapply(SAheart[1:309,-10], as.numeric)
trainN.target = SAheart[1:309,10]

testN = sapply(SAheart[310:462,-10], as.numeric)
testN.target = SAheart[310:462,10]

## assign a column of ones to the training data
trainNB = cbind(numeric(nrow(trainN)), trainN)
trainNB[,1] = 1
testNB = cbind(numeric(nrow(testN)), testN)
testNB[,1] = 1

## initialize a coeff. vector 
beta <- rnorm(ncol(train), mean = 0, sd = .5)

## define a likelihood function
likelihood <-function(beta, y, x){
  return(-sum(y*(x %*%beta - log(1+exp(x %*% beta))) + (1-y)*(-log(1 + exp(x %*% beta))))) 
}

##find the coefficents with optim function 
logit <- optim(beta, likelihood, x = trainNB, y = trainN.target, method = 'BFGS', hessian=TRUE)
logit
##transfer optimized parameters 
beta <-logit$par
trainN.target[1:10]
exp(t(beta)%*%trainNB[6,])/(1+exp(t(beta)%*%trainNB[6,])) > 1/(1+exp(t(beta)%*%trainNB[6,]))

## test against the biult in function for logistic regression 
logitBI <- glm(chd ~. , data = as.data.frame(train),family = binomial(link= "logit"))
dummy = logitBI$coefficients

## set up loop variables 
predictionMine = predictionBI = numeric(nrow(testN))
zeroCorrect = oneCorrect = 0
zeroCorrectBI = oneCorrectBI = 0

## test the predictive ability of builtin's parameters 
for(i in 1:nrow(testNB)){
  if( exp(t(beta)%*%testNB[i,])/(1+exp(t(beta)%*%testNB[i,])) < 1/(1+exp(t(beta)%*%testNB[i,]))){
    predictionMine[i] = 0
    if(testN.target[i] == 0){ 
      zeroCorrect = zeroCorrect + 1  
    }
  }
  else{
    predictionMine[i] = 1 
    if(testN.target[i] == 1)
      oneCorrect = oneCorrect + 1
  }
}



## test the predictive ability of builtin's parameters 
for(i in 1:nrow(testNB)){
  if( exp(t(dummy)%*%testNB[i,])/(1+exp(t(dummy)%*%testNB[i,])) < 1/(1+exp(t(dummy)%*%testNB[i,]))){
    predictionBI[i] = 0
    if(testN.target[i] == 0){ 
      zeroCorrectBI = zeroCorrectBI + 1  
    }
  }
  else{
    predictionBI[i] = 1 
    if(testN.target[i] == 1)
      oneCorrectBI = oneCorrectBI + 1
    }
  }

##create table of prediction data 
a<- data.frame(table(testN.target))
a <- cbind(a,numberCorrect=1)
a <- cbind(a,builtInCorrect=1)
a[2,3] = zeroCorrect
a[1,3] = oneCorrect
a[2,4] = zeroCorrectBI
a[1,4] = oneCorrectBI
a[,1] = t(cbind(2,3))
a.table <- xtable(a, "Logistic Regression on the South African Cancer Data")
a.table

plot(logitBI)

