## Read in cancer data
X = (read.table("~/prostate.data"))

## Seperate train and test data
X.train = X[X[,10],-c(9,10)]
Y = X[X[,10],9]
X.test = X[!X[,10],]

## Scale the test and train data 
X.trainScl = scale(X.train, TRUE, TRUE)
X.testScl = scale(X.test, TRUE, TRUE)
X.trainScl = X.trainScl[,-1]

## compute the linear model for each of the subsets 
alpha = matrix(0,ncol(X.trainScl),1)

E = t(X.trainScl %*% alpha - Y) %*% (X.trainScl %*% alpha - Y)

errorFunction <- function(X, Y, alpha){
  E = t(X %*% alpha - Y) %*% (X %*% alpha - Y)
  return(E)
}
errorFunction(X.trainScl,Y,alpha)
