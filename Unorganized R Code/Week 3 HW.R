X <- as.matrix(read.table(gzfile("~/zip.train.gz"))) ##read in data 
y = matrix(FALSE,10,7291)
X.means = matrix(NA, 10, 256)
X.covars = array(NA, c(256,256,10))
prior =  numeric(10)
N = dim(X)[1]

## loop through all data sorting for classes
## Compute the column mean of each class 
## And the covarience matrix 
for(i in 1:10){
  y[i,] = X[,1] == i-1   
}

count = numeric(10)
## Find the coulumn mean for each class, save it to X.means[] 
## also calculate the prior probability = #ofclass/totalSampleSize
for(i in 1:10){
X.temp <- X[y[i,],-1]
count[i] = dim(X.temp)[1]
prior[i] = (dim(X.temp)[1]/N)
X.means[i,] <- colMeans(X.temp)
}

sum = matrix(0, 256,256)
z = matrix(0, 10,2)
## Calculate the coarience matix
for(i in 1:10) ##loop over each class
  {
  sum = 0 
  X.temp <- X[y[i,],-1]
  z[i,] = dim(X.temp)    ##get numbers of members of the class
  for(j in 1:z[i,1]) ##loop over each of the members of class 
  {                ##sum over all of the rows of the covarience matrix
   sum = sum + ((X.temp[j,] - X.means[i]) %*% t(X.temp[j,] - X.means[i]))
  }
  sum = sum/z[i,1]   ##devide by the number of items in the class
  X.covars[,,i] = sum  ##add the covarience matrix to the array  
}

X <- as.matrix(read.table(gzfile("~/zip.test.gz"))) ##read in data 
y = matrix(FALSE,10,2007) ##

for(i in 1:10){ ## Loop through the first column of the test file for the values 
   y[i,] = X[,1] == i-1 
  } 

alpha =  .3*diag(256) ## create the matrix offset 
prediction = numeric(10) ##create your prediction vector for each test input
classification = numeric(2007) ##create your classification vector 

for(j in 1:2007){ ##loop through each item in the test file 
  x= X[j,-1]      ##assign a temp 
  for(i in 1:10){ ##loop through each class 
   prediction[i] =  prior[i]*((1/sqrt(det(2*pi*X.covars[,,i]+alpha)))*exp(-1/2*t(x - X.means[i])%*%solve(X.covars[,,i]+alpha)%*%(x-X.means[i])))
  }
  classification[j] = which.max(prediction)-1 ##assign the maximum prediction to the classification 
}


##Create Confusion Matrix
confusionMatrix = matrix(0,10,10)
for(i in 1:2007){ ##loop through each item in test file
  if(classification[i] == X[i,1]){ ##if classification worked, add to the X,X spot in the matrix
    confusionMatrix[X[i,1]+1,X[i,1]+1] = confusionMatrix[X[i,1]+1,X[i,1]+1] + 1
  }
  else{ ## if not add to the X,Classification spot in the confusion matrix 
    confusionMatrix[X[i,1]+1, classification[i]+1] = confusionMatrix[X[i,1]+1, classification[i]+1] + 1
  } 
}
##Set up confusion matrix as a table 
CM = (as.table(confusionMatrix))
rownames(CM) = colnames(CM) = c(0,1,2,3,4,5,6,7,8,9)
write.csv(CM, 'confusionmatirx.csv')
