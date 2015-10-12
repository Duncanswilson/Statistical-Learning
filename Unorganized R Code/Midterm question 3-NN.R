## read in data as numeric values (due to bool) 
trainData <- sapply( SAheart[1:309,], as.numeric)
testData = sapply(SAheart[310:462,-10], as.numeric)
test.target = SAheart[310:462,10]
mydata <- data.frame(trainData, ldh) 

#Train the neural network
net.SAheart <- neuralnet(chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age, trainData, 3, 0.0001, stepmax = 1000000000)

#Run the training data through the neural network 
##(this still gives weird results, even though the function says it converges to an error of 0.0001)
net.testResults <- compute(net.SAheart, trainData[,-10]) 
cleanoutput <- cbind(trainData[,10], net.testResults$net.result)

#Plot the neural network
plot(net.SAheart)

#Run the test data through the neural network
net.results <- compute(net.SAheart, testdata) 

#Lets see what properties the network has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,test.target,
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
net.results$net.result[2]
print(cleanoutput)

## calculate the error term 
oneCorrect = oneCounter = zeroCorrect = zeroCounter = i = 0 
for(i in 1:length(test.target)){ 
  if(abs(1- net.results$net.result[i]) < abs(net.results$net.result[i])){
     oneCounter = oneCounter + 1
     if(test.target[i] == 1){
       oneCorrect = oneCorrect + 1 
     }
    }
  else{
    zeroCounter = zeroCounter + 1 
    if(test.target[i] == 0){
      zeroCorrect = zeroCorrect +1
    }
  }
}

## make table of results 
a<- data.frame(table(train.target))
a <- cbind(a,builtInCorrect=1)
a[2,3] = zeroCorrect
a[1,3] = oneCorrect
a.table <- xtable(a, "Neural Network on the South African Cancer Data")
a.table

