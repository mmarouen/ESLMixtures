generateXII<-function(Ntrain=200,Ntest=1000,p=4){
  N1=floor(Ntrain/2)
  N1test=floor(Ntest/2)
  #generate responses
  y1=rep(1,length=N1)
  y2=rep(-1,length=N1)
  y1test=rep(1,N1test)
  y2test=rep(-1,N1test)
  #generate training data
  train1=matrix(rnorm(N1*p),ncol=p,nrow=N1)
  n1=N1*20
  tt=matrix(rnorm(n1*p),ncol=p,nrow=n1)
  v1=apply(tt,1,function(x) sum(x^2))
  t2=tt[v1>9 & v1<16,]
  inds=sample(1:nrow(t2),size = N1,replace = FALSE)
  train2=t2[inds,]
  yTrain=c(y1,y2)
  train=rbind(train1,train2)
  shuffle=sample(1:(2*N1),size=2*N1,replace=FALSE)
  train=train[shuffle,]
  yTrain=yTrain[shuffle]
  #generate test data
  test1=matrix(rnorm(N1test*p),ncol=p,nrow=N1test)
  n2=N1test*20
  tt=matrix(rnorm(n2*p),ncol=p,nrow=n2)
  v1=apply(tt,1,function(x) sum(x^2))
  t2=tt[v1>9 & v1<16,]
  inds=sample(1:nrow(t2),size = N1test,replace = FALSE)
  test2=t2[inds,]
  yTest=c(y1test,y2test)
  test=rbind(test1,test2)
  shuffle=sample(1:(2*N1test),size=2*N1test,replace=FALSE)
  test=test[shuffle,]
  yTest=yTest[shuffle]
  
  return(list(train=train,yTrain=yTrain,test=test,yTest=yTest))
}