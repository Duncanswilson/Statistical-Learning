X <- as.matrix(read.table(gzfile("~/zip.train.gz"))) ##read in data 
y = matrix(FALSE,10,7291)
X.means = matrix(NA, 10, 255)
X.covars = array(NA, c(255,255,10))
X.projectCovars = array(NA, c(16,16,10))
prior =  numeric(10)
N = dim(X)[1]
z = matrix(0, 10,2)

## loop through all data sorting for classes
for(i in 1:10){
  y[i,] = X[,1] == i-1   
}

##Seperate and mean scale each class 
for(i in 1:10){
X[y[i,],-1] = scale(X[y[i,],-1], TRUE,FALSE)
}

count = numeric(10)
## Find the coulumn mean for each class, save it to X.means[] 
## also calculate the prior probability = #ofclass/totalSampleSize
for(i in 1:10){
X.temp <- X[y[i,],c(-1,-257)]
count[i] = dim(X.temp)[1]
prior[i] = (dim(X.temp)[1]/N)
X.means[i,] <- colMeans(X.temp)
}

## Calculate the coarience matix
for(i in 1:10)
  { ##loop for each class
  sum = 0 
  X.temp <- X[y[i,],c(-1,-257)]
  covar <- cov(X.temp)
  X.covars[,,i]= covar  ##add the covarience matrix to the array     
}

P = array(NA, c(255,255,10))
##Compute eigenvalues of each class: 
for(i in 1:10){
  P[,,i] =  (eigen(X.covars[,,i])$vectors)
}

X.project<- matrix(0,dim(X)[1],16)   ##get numbers of members of the class

for(i in 1:10){
  X.project[y[i,],] <- X[y[i,],c(-1,-257)] %*% (P[,,i][,1:16])
}

## Calculate the coarience matix
for(i in 1:10)
{ ##loop for each class
  sum = 0 
  X.temp <- X.project[y[i,],]
  covar <- cov(X.temp)
  X.projectCovars[,,i]= covar  ##add the covarience matrix to the array     
}

X <- as.matrix(read.table(gzfile("~/zip.test.gz"))) ##read in data 
y = matrix(FALSE,10,dim(X)[1])

## loop through all data sorting for classes
for(i in 1:10){
  y[i,] = X[,1] == i-1   
}

##Seperate and mean scale each class 
for(i in 1:10){
  X[y[i,],-1] = scale(X[y[i,],-1], TRUE,FALSE)
}

X.projectTest<- matrix(0,dim(X)[1],16)   ##get numbers of members of the class

##Project the test data onto the eigenvectors
for(i in 1:10){
  X.projectTest[y[i,],] <- X[y[i,],c(-1,-257)] %*% (P[,,i][,1:16])
}

alpha =  .1*diag(16) ## create the matrix offset 
prediction = numeric(10) ##create your prediction vector for each test input
classification = numeric(dim(X)[1]) ##create your classification vector 

for(j in 1:2007){ ##loop through each item in the test file 
  x = X.projectTest[j,]      ##assign a temp 
  for(i in 1:10){ ##loop through each class 
    prediction[i] =  ((1/sqrt(det(2*pi*X.projectCovars[,,i]+alpha)))*exp(-1/2*t(x)%*%solve(X.projectCovars[,,i]+alpha)%*%(x)))
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

write.table(confusionMatrix, file = "confusionMatrix.csv", sep = " ", row.names = 0:9, col.names = 0:9)

sum(diag(confusionMatrix))/2007
dim(t(x))

