#calling function which aggregates all mixtures in the book

callMixtures<-function(chap="2",order="1",input1=NULL,input2=NULL,input3=NULL,Ntrain=NULL,Ntest=NULL,p=NULL,N=NULL,
                       size=NULL,Nfeat=NULL,K=NULL){
  ll=list()
  out1=list()
  out2=list()
  if(chap=="2"){
    source("D:/RProject/DataRepository/ESL/ESLMixtures/chapII_Mixtures.R")
    if(order=="1"){
      if(!is.null(Ntrain) & !is.null(Ntest)){out1=chapII_mix(Ntrain=Ntrain,Ntest=Ntest)}
      if(!is.null(Ntrain) & is.null(Ntest)){out1=chapII_mix(Ntrain=Ntrain)}
      if(is.null(Ntrain) & !is.null(Ntest)){out1=chapII_mix(Ntest=Ntest)}
      if(is.null(Ntrain) & is.null(Ntest)){out1=chapII_mix()}
      ll=list(train=out1$train,resp=out1$resp,test=out1$test,respTest=out1$respTest,
              BlueCentroids=out1$BlueCentroids,OrangeCentroids=out1$OrangeCentroids)
    }
    if(order=="2"){
      out2=chapII_bayesianBoundary(input1,input2,input3)
      ll=list(yhat_bayes=out2)
    }
  }
  return(ll)
}