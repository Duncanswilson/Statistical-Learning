train <- subset( prostate, train==TRUE )[,1:9]
test <- subset( prostate, train=FALSE )[,1:9]

net.prostate <- neuralnet(lpsa ~   lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, train, hidden=5, threshold=0.01)

plot(net.prostate)

net.result = compute(net.prostate, test[,-9])

fittingResults <- (cbind(train[,9],net.result$net.result))
error = (test[,9] - net.result$net.result)
         
plot(error, ylab = "Neural Network Error", xlab = "Index in test set")
