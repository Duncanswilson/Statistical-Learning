#import test and train data
X.train <- as.matrix(read.table(gzfile("~/zip.train.gz")))
X.test <- as.matrix(read.table(gzfile("~/zip.test.gz")))

#Seperate the 2's and 3's of training data  
SevensandNines <- (which(X.train[,1] == 7 | X.train[,1] == 9))
Y.train <- (X.train[SevensandNines, 1]-7) ##possibly could break things 
for(i in 1:length(Y.train)){
  if(Y.train[i] == 2 ){
    Y.train[i] = 1 
  }
}
X.subset = X.train[SevensandNines,]
X.subset[,1] = 1

#set up sigmoid function 
sigmoid <- function(weights,data){
  sig <- (1/(1+exp(-t(weights)%*%data)))
  return(sig) 
}

#iterate through data 
neuron <- function(weights, data){
  output <- numeric(nrow(data))
  for(i in 1:nrow(data)){
    output[i] <- sigmoid(weights, data[i,])
  }
  return(output)
}

##calculate the error
errorFunc <- function(output, target){
  error = 0 
  for(i in 1:length(output)){
    error <- error + (target[i]- output[i])^2
  }
  error = 1/2*error
  return(error)
}

##calculate the gradient 
gradient <- function(weights,data,target){
  grad <- numeric (length(weights))
  for(j in 1:length(weights)){
    for(i in 1:ncol(data)){
      grad[j] <- grad[j] + ((sigmoid(weights,data[i,]) - target[i])*sigmoid(weights,data[i,])*(1-sigmoid(weights,data[i,]))*data[i,j])
    }
  }
  return(grad)
}

##update weights 
updateWeights <- function(grad, weights){
  weights = weights - gamma*grad 
  return(weights)
}
##################################
##initialize weights and step size 
beta = rnorm(ncol(X.subset), mean = 0, sd = .5)
gamma = .2 

##prime the loop 
output <- neuron(beta,X.subset)
error <- errorFunc(output, Y.train)
error
grad <- gradient(beta,X.subset,Y.train)
beta <- updateWeights(grad, beta)

##train the network over the train data 
while((mean((output - Y.train)^2) > .01)){
  output <- neuron(beta,X.subset)
  error <- errorFunc(output, Y.train)
  grad <- gradient(beta,X.subset,Y.train)
  beta <- updateWeights(grad, beta)
}

#Seperate the 2's and 3's of test data  
SevensandNines.test <- (which(X.test[,1] == 7 | X.test[,1] == 9))
Y.test <- (X.test[SevensandNines.test, 1] - 7 )  
for(i in 1:length(Y.test)){
  if(Y.test[i] == 2 ){
    Y.test[i] = 1 
  }
}


X.testSubset = X.test[SevensandNines.test,]
X.testSubset[,1] = 1

#run test data through the neuron 
output <- neuron(beta,X.testSubset)


##compare the output of the netwok to the test data
correctSevens = 0
correctNines = 0 
for(i in 1:length(output)){
  if(abs(1-output[i]) < abs(0-output[i])){
    output[i] = 1
  }
  else{
    output[i] = 0
  }
  if(Y.test[i] == 0 && Y.test[i] == output[i]){
    correctSevens = correctSevens + 1 
  }
  
  if(Y.test[i] == 1 && Y.test[i] == output[i]){
    correctNines = correctNines + 1 
  }
}

## make the table of the output data 
a <- data.frame(table(Y.test))
a <- cbind(a,numberCorrect=1)
a[1,3] = correctSevens
a[2,3] = correctNines
a[,1] = t(cbind(7,9))
a.table <- xtable(a, "7's vs. 9's")
a.table

