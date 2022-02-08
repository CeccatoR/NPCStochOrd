ad<-function(x,label){

  ni<-table(label)
  W=table(x,label)
  k=dim(W)[1]
  f1=W[,1]
  f2=W[,2]
  f=f1+f2

  F1=cumsum(f1)[1:(k-1)]/ni[1]
  F2=cumsum(f2)[1:(k-1)]/ni[2]
  F=cumsum(f)[1:(k-1)]/sum(ni)

  T_ad=sum((F2-F1)/sqrt(F*(1-F)))

  T_ad
}
