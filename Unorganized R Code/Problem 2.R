library(class)
##Read in Training Data
X <- as.matrix(read.table(gzfile("C:/Users/d/Dropbox/Rfiles/zip.train.gz")))
TwosandThrees <- (which(X[,1] == 2 | X[,1] == 3))
X.train <- X[TwosandThrees, -1]
y.train <- X[TwosandThrees, 1] == 2 
y.trainThrees <- X[TwosandThrees, 1] == 3
##Read in Test Data
X <- as.matrix(read.table(gzfile("C:/Users/d/Dropbox/Rfiles/zip.test.gz")))
TwosandThrees <- which(X[,1] == 2 | X[,1] == 3)
X.test <- X[TwosandThrees, -1]
y.test <- X[TwosandThrees, 1] == 2

L <- lm(y.trainThrees ~ X.train)

yhatTrain <- (cbind(1, X.train) %*% L$coef) >= 0.5 

diff <- yhatTrain != y.train
twoDiff <- diff != y.trainTwos

TwosandThrees
L.trainerror <- mean(yhatTrain != y.train)

yhatTest <- (cbind(1, X.test) %*% L$coef) >= 0.5 
L.testerror <- mean(yhatTest != y.test)



##K-nearest neighbors
k <- c(1,3,5,7,15)
k.error <- c(NA,NA,NA,NA,NA)
for(i in 1:length(k)){
  yhat <- knn(X.train, X.test, y.train, k[i])
  k.error[i] <- mean(yhat != y.test)
}

##Make and print table 
totalError = matrix(c(L.error, k.error), ncol = 1) 
colnames(totalError) = "Rate of Error"
rownames(totalError) = c("Linear Regression", paste0("k-NN with k =", k))
totalError

