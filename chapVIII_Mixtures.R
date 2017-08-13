generateVIII <-function(N=50){
  library(splines)
  x=runif(N,0,3)
  x=sort(x)
  f_x=-x^2*(x-1.3)*(x-2)*(x-3.1)
  eps=rnorm(N,0,1)
  y=f_x+eps
  X=bs(x,knots = c(0.75,1.5,2.25),df = 7,intercept = TRUE)
  return(list(f_x=f_x,y=y,x=x,X=X))
}

generateVIII_2<-function(N=70,Ntest=2000,p=5){
  N=30
  p=5
  library(MASS)
  sigma=matrix(0.95,ncol=p,nrow = p)
  diag(sigma)=1
  input=mvrnorm(N,rep(0,p),sigma)
  yTrain=rep(0,length=N)
  yTrain[input[,1]<=0.5]=sample(c(0,1),size = sum(input[,1]<=0.5),replace = T,prob = c(0.8,0.2))
  yTrain[input[,1]>0.5]=sample(c(0,1),size = sum(input[,1]>0.5),replace = T,prob = c(0.2,0.8))
  test=mvrnorm(Ntest,rep(0,p),sigma)
  yTest=rep(0,length=Ntest)
  yTest[test[,1]<=0.5]=sample(c(0,1),size = sum(test[,1]<=0.5),replace = T,prob = c(0.8,0.2))
  yTest[test[,1]>0.5]=sample(c(0,1),size = sum(test[,1]>0.5),replace = T,prob = c(0.2,0.8))
  return(list(yTrain=yTrain,train=input,yTest=yTest,test=test))
}