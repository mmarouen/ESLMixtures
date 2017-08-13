#generate 2D Mixtures of Gaussians for 3 classes
#each class data is a mixture of 10 gaussians
#class centroids are sampled from a gaussian distribution
#Input: 
#N=number of observation in each class, default=100
#K=number of classes, default=3
#Output:
#1 matrix: (N*K rows) x (3Cols: X1=coord1,X2=coord2,Y=class number)
gaussianMix<-function(N=100,K=3){
  library(MASS)
  
  #Centroids distribution
  onesI=mvrnorm(n = 6, mu = c(0.2,2),Sigma = 0.5*diag(2)) #class1_I
  onesII=mvrnorm(n= 4, mu = c(1.7,2),Sigma = 0.5*diag(2)) #class1_II
  twoI=mvrnorm(n = 6,  mu = c(1,1),Sigma = 0.5*diag(2)) #clas2_1
  twoII=mvrnorm(n = 4, mu = c(1.5,1),Sigma = 0.5*diag(2)) #class2_2
  threeI=mvrnorm(n = 6,mu = c(0.2,0),Sigma = 0.5*diag(2)) #class3_1
  threeII=mvrnorm(n=4, mu = c(1.7,0),Sigma = 0.5*diag(2)) #class3_2
  
  #sample N observation per class
  #N=100
  IndsI=sample(1:6,size = floor(N*0.6),replace = TRUE)
  IndsII=sample(1:4,size=floor(N*0.4),replace = TRUE)
  classI=t(apply(as.matrix(1:N),1,function(x){
    if(x<= floor(N*0.6))return(mvrnorm(1,onesI[IndsI[x],],0.7*diag(2)))
    if(x>floor(N*0.6)) return(mvrnorm(1,onesII[IndsII[x-floor(N*0.6)],],0.7*diag(2)))
  }))
  classI=cbind(classI,1)
  
  classII=t(apply(as.matrix(1:N),1,function(x){
    if(x<= floor(N*0.6)) return(mvrnorm(1,twoI[IndsI[x],],0.3*diag(2)))
    if(x>floor(N*0.6)) return(mvrnorm(1,twoII[IndsII[x-floor(N*0.6)],],0.5*diag(2)))
  }))
  classII=cbind(classII,2)
  
  classIII=t(apply(as.matrix(1:N),1,function(x){
    if(x<= floor(N*0.6))return(mvrnorm(1,threeI[IndsI[x],],0.7*diag(2)))
    if(x>floor(N*0.6)) return(mvrnorm(1,threeII[IndsII[x-floor(N*0.6)],],0.7*diag(2)))
  }))
  classIII=cbind(classIII,3)
  df=rbind(classI,classII,classIII)
  colnames(df)=c("X1","X2","labels")
  df=as.data.frame(df)
  return(list(train=df[,-3],yTrain=df$labels))
}

bayesBoundary<-function(Input,cent){
  Input=as.matrix(Input)
  cent=as.matrix(cent)
  
  return(yHatBayes)
}