##read in data 
train <- subset( prostate, train==TRUE )[,1:9]
test <- subset( prostate, train=FALSE )[,1:9]

## fit a linear model 
fit <- lm(lpsa ~   lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, train)

## explore the predicted data 
cbind(fitted(fit),train[,9])
plot(fit)

## use the updated coeff to predict the test data 
testResults <- predict(fit, test[,-9])

## assign the error term 
error <- (test[,9]-testResults)

## plot the error 
plot(error, ylab = "Error of Linear Regression", xlab = "Index in test set")
