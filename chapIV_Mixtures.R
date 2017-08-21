chapIV_Mixture1 <- function(size=300){
  library(MASS)
  s <- sqrt(1/500)
  s1 <- sqrt(1/350)
  N=floor(size/3)
  ones=mvrnorm(10,c(-1/2,0),matrix(c(3*s1,0,0,s1),2,2))
  twos=mvrnorm(10,c(-1,1),diag(2)*s1)
  threes=mvrnorm(10,c(1,.5),matrix(c(s1/10, 0,0, s1*4),2,2))
  indOnes=sample(1:10,N,replace = TRUE)
  indTwos=sample(1:10,N,replace = TRUE)
  indThrees=sample(1:10,N,replace = TRUE)
  obsOnes=matrix(0,nrow = N,ncol = 3)
  obsOnes[,3]=1
  obsTwos=matrix(0,nrow = N,ncol = 3)
  obsTwos[,3]=2
  obsThrees=matrix(0,nrow = N,ncol = 3)
  obsThrees[,3]=3
  for(i in 1:N){
    obsOnes[i,1:2]=mvrnorm(1,mu = ones[indOnes[i],],diag(2)*s)
    obsTwos[i,1:2]=mvrnorm(1,mu = twos[indTwos[i],],diag(2)*s)
    obsThrees[i,1:2]=mvrnorm(1,mu = threes[indThrees[i],],diag(2)*s)
  }
  data=rbind(obsOnes,obsTwos,obsThrees)
  inds=sample(3*N,3*N)
  data=data[inds,]
  yTrain=data[,3]
  train=data[,1:2]
  return(list(train=train,yTrain=yTrain))
}

