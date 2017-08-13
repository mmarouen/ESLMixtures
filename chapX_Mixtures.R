##10 features: standard independant gaussian
##response = 
##+1 if sum(Xi^2)>chisquared_10(0.5)
##-1 else
generateX<- function(Ntrain=2000,Nfeat=10,Ntest=10000){
  train=matrix(0,ncol = Nfeat,nrow = Ntrain)
  test=matrix(0,ncol=Nfeat,nrow = Ntest)
  for (j in 1:Nfeat){
    train[,j]=rnorm(Ntrain,0,1)
    test[,j]=rnorm(Ntest,0,1)
  }
  yTrain=rep(-1,length=Ntrain)
  yTest=rep(-1,length=Ntest)
  threshold=qchisq(0.5,df = Nfeat)
  inds=rowSums(train^2)>threshold
  inds2=rowSums(test^2)>threshold
  yTest=rep(-1,length=Ntest)
  yTrain[inds]=1
  yTest[inds2]=1
  return(list(train=train,yTrain=yTrain,test=test,yTest=yTest))
}