#Aim:
#- Computation of p-values via NonParametric Combination methodology for 2-sample univariate and multivariate stochastic ordering problems.

#Input:
#- y: data.frame containing group labels (in the first column) and V outcome variables
#- statistic: "dm" for difference in mean, "ad" for Anderson-Darling test statistic (see Pesarin and Salmaso, 2010). Default: "dm"
#- alternative: an integer specifying the alternative hypothesis, must be 1 ("greater") or -1 ("less"). Default: 1
#- B: number of permutations (B > 2000 is recommended)
#- comb.fun: combining function to be used (e.g. "F" for Fisher's)
#- seed: the seed to be set for reproducibility

#Output:
#- P.comb: combined p-values
#- P.part: partial p-values adjusted using FWE.minP correction


test.stat.2.samples <- function(y, statistic = "dm", alternative = 1, B = 2000, comb.fun = "F"){

  # #to combine
  # source("comb.R")
  # #to compute p-values
  # source("t2p.R")
  # #to adjust p-values
  # source("FWE.minP.R")

  label.orig <- y[,1]
  size <- table(label.orig)
  label = vector(length=length(label.orig))
  label[label.orig==unique(label.orig)[1]] <- 1
  label[label.orig==unique(label.orig)[2]] <- 2

  #Outcome variables
  obs <- y[,2:ncol(y),drop=FALSE]
  T <- array(dim=c((B+1),ncol(obs)))
  dim(T) <- c((B+1),ncol(obs))

  #Test statistic
  if(statistic=="dm"){
    #source("dm.R")
    tstat<-dm
  }else if(statistic=="ad"){
    #source("ad.R")
    tstat<-ad
  }else{
    return("Wrong test statistic")
  }


  #Test statistic - observed value
  T[1,] <- apply(obs, 2, function(x) tstat(x,label))

  #Test statistic - permutation distribution
  for(bb in 2:(B+1)){
    u <- sample(1:dim(obs)[1], dim(obs)[1])
    obs.star <- obs[u,,drop=FALSE]#cbind(label, obs[u,1:dim(obs)[2]])
    T[bb,] <-apply(obs.star, 2, function(x) tstat(x,label))
  }

  T <- T*sign(alternative)

  #p-value calculation
  Pv <- t2p(T)

	#combination step: combine insights from different outcome variables
	if((ncol(y)-1)>1){
	  #combine unadjusted p-values
	  T.comb<-comb(Pv,fcomb=comb.fun)
	  #compute global p-values
	  P.comb<-t2p(T.comb)
	  #Adjust partial p-values for multiplicity
	  P.part<-t(FWE.minP(Pv))
	}else{
	  P.comb<-Pv
	  P.part<-Pv
	}


	return(list(P.comb=P.comb[1,],P.part=P.part[1,]))
}

