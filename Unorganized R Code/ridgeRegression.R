##Read in Data 
X = (read.table("~/prostate.data"))

## Seperate train and test data
X.train = X[X[,10],-c(9,10)]
Y = X[X[,10],9]
X.test = X[!X[,10],-c(9,10)]
plot(X.train) 

## Scale the test and train data 
X.trainScl <- scale(X.train, TRUE, TRUE)
X.testScl <- scale(X.test, TRUE, TRUE)

##data to fit our model to 
X.lpsa = X.testScl[,9]

linearModel <- lm(X.lpsa~X.testScl)
rss <- sum(linearModel$resid^2/linearModel$df.residual)
X.beta <- coef(linearModel)[-1]

dim(X.testScl)
##creat idenity matrix and loop over all the possible lamda values 
I <- diag(8)
error = numeric(1000)
lamda = 0
for(lamda in 1:10000){
beta =  solve(t(X.testScl)%*%X.testScl+lamda*I)%*%t(X.testScl)%*%X.lpsa
error[lamda] = (X.lpsa-t(X.testScl%*%beta))%*%(X.lpsa-(X.testScl%*%beta))+lamda*(t(beta)%*%beta)
}
error1 = (X.lpsa-t(X.testScl%*%X.beta))%*%(X.lpsa-(X.testScl%*%X.beta))+lamda*(t(X.beta)%*%X.beta)
lamda = numeric(10001)

png("~/RSS&lamda.png")
plot(error, type = 'l' , ylab = "Ressidual Sum of Squares", xlab = "Lamda Value",lwd=5, col ='palevioletred2')
dev.off()
