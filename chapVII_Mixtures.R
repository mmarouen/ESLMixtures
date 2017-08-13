generateVII <-function(p=20,N=80){
  train=matrix(runif(N*p,0,1),ncol=p)
  yleft=rep(0,length=N)
  yright=rep(0,length=N)
  yleft[train[,1]>0.5]=1
  yright[rowSums(train[,1:10])>5]=1
  return(list(train=train,yTright=yright,yTleft=yleft))
}
