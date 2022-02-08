dm<-function(x,label){

  T_dm=mean(x[label==1])-mean(x[label==2])

  T_dm
}
