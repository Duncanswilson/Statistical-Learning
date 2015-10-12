# Read in the training data
X <- as.matrix(read.table(gzfile("C:/Users/d/Dropbox/Rfiles/zip.train.gz")))
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.train <- X[y2or3, -1]
y.train <- X[y2or3, 1] == 2

# Read in the test data
X <- as.matrix(read.table(gzfile("C:/Users/d/Dropbox/Rfiles/zip.test.gz")))
y2or3 <- which(X[, 1] == 2 | X[, 1] == 3)
X.test <- X[y2or3, -1]
y.test <- X[y2or3, 1] == 2

# Classification by linear regression
L <- lm(y.train ~ X.train)
yhatTest <- (cbind(1, X.test) %*% L$coef) >= 0.5
yhatTrain <- (cbind(1, X.train) %*% L$coef) >= 0.5
L.testError <- mean(yhatTest != y.test)
L.trainError <- mean(yhatTrain != y.train)
L.testError
L.trainError

