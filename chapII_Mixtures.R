#generates 2 multivariate gaussians with same covariance matrix and different means
#associates each generated data with a class
chapII_mix <- function(Ntrain=200,Ntest=10000){
  library(MASS)
  blue=mvrnorm(10,c(1,0),diag(2))#blue centroids
  orange=mvrnorm(10,c(0,1),diag(2))#orange centroids
  ###train data
  N1=Ntrain/2
  ind_blue=sample(1:10,N1,replace = TRUE)
  ind_orange=sample(1:10,N1,replace = TRUE)
  
  blueOBS=t(apply(as.matrix(ind_blue),1,function(x) mvrnorm(1,mu = blue[x,],0.2*diag(2))))
  orangeOBS=t(apply(as.matrix(ind_blue),1,function(x) mvrnorm(1,mu = blue[x,],0.2*diag(2))))
  
  bluedata=data.frame(blueOBS,class="blue")
  orangedata=data.frame(orangeOBS,class="orange")
  train=rbind(bluedata,orangedata)
  inds=sample(2*N1,2*N1)
  train=train[inds,]
  levels(train$class)=c(0,1)
  yTrain=train$class
  train$class=NULL
  ###test data
  N2=Ntest/2
  indTest_blue=sample(1:10,N2,replace = TRUE)
  indTest_orange=sample(1:10,N2,replace = TRUE)
  blueTestOBS=t(apply(as.matrix(indTest_blue),1,function(x) mvrnorm(1,mu = blue[x,],0.2*diag(2))))
  orangeTestOBS=t(apply(as.matrix(indTest_orange),1,function(x) mvrnorm(1,mu = orange[x,],0.2*diag(2))))
  blueTestdata=data.frame(blueTestOBS,class="blue")
  orangeTestdata=data.frame(orangeTestOBS,class="orange")
  test=rbind(blueTestdata,orangeTestdata)
  inds=sample(2*N2,2*N2)
  test=test[inds,]
  levels(test$class)=c(0,1)
  yTest=test$class
  test$class=NULL
  return(list(train=train,resp=yTrain,test=test,respTest=yTest,BlueCentroids=blue,OrangeCentroids=orange))
}


chapII_bayesianBoundary<- function(Input,blue,orange){
  blue=as.matrix(blue)
  orange=as.matrix(orange)
  Input=as.matrix(Input)
  P_blue=rowSums(apply(blue,1,function(x) exp(-(5/2)*rowSums(t(t(Input)-x)^2))))
  P_orange=rowSums(apply(orange,1,function(x) exp(-(5/2)*rowSums(t(t(Input)-x)^2))))

  yhat_bayes=as.numeric(P_blue > P_orange)
  return(yhat_bayes)
}