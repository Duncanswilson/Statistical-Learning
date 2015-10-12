## import data
train <- subset( prostate, train==TRUE )[,1:9]
test <- subset( prostate, train=FALSE )[,1:9]

## train neural net model with 5 hidden layers
net.prostate <- neuralnet(lpsa ~   lcavol + lweight + age + lbph + 
							svi + lcp + gleason + pgg45, train, hidden=5, threshold=0.01)

## plot the network 
plot(net.prostate)

## run the test data through 
net.result = compute(net.prostate, test[,-9])

## calculate error
fittingResults <- (cbind(train[,9],net.result$net.result))
error = (test[,9] - net.result$net.result)
         
## plot error 
plot(error, ylab = "Neural Network Error", xlab = "Index in test set")
