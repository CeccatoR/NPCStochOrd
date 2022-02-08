test.stat.C.samples <- function(y, ord, statistic = "dm", B = 2000, comb.fun = "F", seed = 1234){

  # #to combine
  # source("comb.R")
  # #to compute p-values
  # source("t2p.R")
  # #to adjust p-values
  # source("FWE.minP.R")

  #test statistic
  if(statistic=="dm"){
    #source("dm.R")
    tstat<-dm
  }else{
    #source("ad.R")
    tstat<-ad
  }

  #save original group labels
  label.orig <- y[,1]

  #check the order
  if(!all(unique(label.orig)%in%ord)){
    print("Wrong order")
    return()
  }

  #sample sizes
  size <- table(label.orig)



  Tg<-array(0,dim=c(B+1,ncol(y)-1,length(size)-1))
  dim(Tg) <- c((B+1),ncol(y)-1,length(size)-1)

  #Pooling and testing
  for(i in 1:(length(size)-1)){

    label = vector(length=length(label.orig))
    #pool first i samples
    label[label.orig%in%ord[1:i]] <- 1
    #pool last C-i samples
    label[label.orig%in%ord[(i+1):length(size)]] <- 2

    #extract outcome variables
    obs <- y[,2:ncol(y)]
    if((ncol(y)-1)==1) dim(obs) <- c(nrow(y),(ncol(y)-1))


    T <- array(dim=c((B+1),ncol(obs)))
    dim(T) <- c((B+1),ncol(obs))

    #Apply Anderson-Darling test statistic (see Pesarin and Salmaso, 2010)
    T[1,] <- apply(obs, 2, function(x) tstat(x,label))

    #Permute B times and estimate permutation distributions
    for(bb in 2:(B+1)){
      set.seed(bb+seed)
      #permute rows to implicitly take into account dependence
      u <- sample(1:dim(obs)[1], dim(obs)[1])
      obs.star <- obs[u,1:dim(obs)[2]]
      if((ncol(y)-1)==1) dim(obs.star) = c(nrow(obs),ncol(obs))
      #Apply Anderson-Darling test statistic on permuted data
      T[bb,] <- apply(obs.star, 2, function(x) tstat(x,label))
    }

    #Compute p-values
    P <- t2p(T);
    Tg[,,i]<-P
  }

  #first combination step: combine insights from different pooling steps
  T.c<-array(0,dim=c(B+1,ncol(y)-1))
  dim(T.c) <- c((B+1),ncol(y)-1)
  if(length(size)>2){
    for(d in 1:(ncol(y)-1)){
      #combine
      T.c[,d] = comb(Tg[,d,],fcomb=comb.fun)
    }
    #compute p-values
    P.c <- t2p(T.c)

  }else{
    P.c <- Tg[,,1]
  }

  #second combination step: combine insights from different outcome variables
  if((ncol(y)-1)>1){
    #combine unadjusted p-values
    T.comb<-comb(P.c,fcomb="T")
    #compute global p-values
    P.comb<-t2p(T.comb)
    #Adjust partial p-values for multiplicity
    P.part<-t(FWE.minP(P.c))
  }else{
    P.comb<-P.c
    P.part<-P.c
  }


  return(list(P.comb=P.comb[1,],P.part=P.part[1,]))
}

